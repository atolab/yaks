open Yaks_core
open Yaks_types
open Caqti_driver
open Be_sql_property

module SQLBE = struct 

  module type Config = sig    
    val id: Apero.Uuid.t 
    val properties : properties
    val conx : connection
  end

  module Make (C : Config) (MVar : Apero.MVar) = struct 

    let properties = C.properties

    let to_string = "SQLBE#"^(Apero.Uuid.to_string C.id)^"{"^(string_of_properties properties)^"}"

    type storage_info =
      {
        path : Path.t
      ; props : properties
      ; table_name : string
      ; schema : string list * Caqti_driver.Dyntype.t
      ; on_dispose : on_dispose
      }

    (************************************************)
    (*   Operations on structured (legacy) tables   *)
    (************************************************)
    let get_sql_table storage_info (selector:Selector.t) =
      if Selector.is_matching storage_info.path selector then
        let%lwt _ = Logs_lwt.debug (fun m -> m "[SQL]: get(%s) from legacy table %s"
                      (Selector.to_string selector) (storage_info.table_name)) in
        let open Lwt.Infix in
        let (col_names, typ) = storage_info.schema in
        Caqti_driver.get C.conx storage_info.table_name typ ?condition:(Selector.query selector) ()
        >|= List.map (fun row -> storage_info.path, Value.SqlValue (row, Some col_names)) 
      else
        let%lwt _ = Logs_lwt.warn (fun m -> m "[SQL]: Can't get Selector %s in Storage with path %s - the exact Storage path is required"
                      (Selector.to_string selector) (Path.to_string storage_info.path))
        in Lwt.return []

    let put_sql_table storage_info (selector:Selector.t) (value:Value.t) =
      if Selector.is_matching storage_info.path selector then
        let%lwt _ = Logs_lwt.debug (fun m -> m "[SQL]: put(%s) into legacy table %s"
                      (Selector.to_string selector) (storage_info.table_name)) in
        let open Value in 
        match transcode value Sql_Encoding with 
        | Ok SqlValue (row, _) -> Caqti_driver.put C.conx storage_info.table_name (String.concat "," row) ()
        | Ok _ -> Lwt.fail @@ YException (`UnsupportedTranscoding (`Msg "Transcoding to SQL didn't return an SqlValue"))
        | Error e -> Lwt.fail @@ YException e
      else
        let%lwt _ = Logs_lwt.warn (fun m -> m "[SQL]: Can't put Selector %s in Storage with path %s - the exact Storage path is required"
                                      (Selector.to_string selector) (Path.to_string storage_info.path))
        in Lwt.return_unit

    let put_delta_sql_table storage_info selector delta =
      let open Apero.LwtM.InfixM in
      if Selector.is_matching storage_info.path selector then
        let%lwt _ = Logs_lwt.debug (fun m -> m "[SQL]: put_delta(%s) into legacy table %s"
                      (Selector.to_string selector) (storage_info.table_name)) in
        get_sql_table storage_info selector
        >>= Lwt_list.iter_p (fun (_,v) -> 
            match Value.update v delta with
            | Ok SqlValue (row, _) ->
              Caqti_driver.put C.conx storage_info.table_name (String.concat "," row) ()
            | Ok _ -> Logs_lwt.warn (
              fun m -> m "[SQL]: put_delta on value %s failed: update of SqlValue didn't return a SqlValue" (Value.to_string v))
            | Error e -> Logs_lwt.warn (
              fun m -> m "[SQL]: put_delta on value %s failed: %s" (Value.to_string v) (show_yerror e)))
      else
        let%lwt _ = Logs_lwt.warn (fun m -> m "[SQL]: Can't put_delta Selector %s in Storage with path %s - the exact Storage path is required"
                                      (Selector.to_string selector) (Path.to_string storage_info.path))
        in Lwt.return_unit

    let remove_sql_table storage_info selector = 
      if Selector.is_matching storage_info.path selector then
        let%lwt _ = Logs_lwt.debug (fun m -> m "[SQL]: remove(%s) from legacy table %s"
                      (Selector.to_string selector) (storage_info.table_name)) in
        match Selector.query selector with
        | Some q -> Caqti_driver.remove C.conx storage_info.table_name q ()
        | None -> 
          let _ = Logs_lwt.debug (fun m -> m "[SQL]: Can't remove selector %s without a query" (Selector.to_string selector)) in
          Lwt.return_unit
      else
        let%lwt _ = Logs_lwt.warn (fun m -> m "[SQL]: Can't remove Selector %s in Storage with path %s - the exact Storage path is required"
                                      (Selector.to_string selector) (Path.to_string storage_info.path))
        in Lwt.return_unit



    (***************************************)
    (*   Operations on key/value  tables   *)
    (***************************************)

    let get_matching_keys storage_info sub_selector =
      let open Apero.LwtM.InfixM in
      Caqti_driver.get_keys_kv_table C.conx storage_info.table_name
          >|= List.filter (fun k -> Selector.is_matching (Path.of_string ~is_absolute:false k) sub_selector)
          >|= fun l -> let _ = Logs_lwt.debug (fun m -> m "[SQL]: in %s found matching keys of %s : %s"
                    (storage_info.table_name) (Selector.to_string sub_selector) (String.concat " " l)) in l


    let get_kv_table storage_info (selector:Selector.t) =
      let%lwt _ = Logs_lwt.debug (fun m -> m "[SQL]: get(%s) from kv table %s"
                    (Selector.to_string selector) (storage_info.table_name)) in
      let open Apero.LwtM.InfixM in
      let (_, typ) = storage_info.schema in
      let sub_sel = Selector.remove_prefix storage_info.path selector in
      let condition_suffix = match Selector.query selector with
        | Some q -> "' AND "^q
        | None -> "'"
      in
      let get_result =
        if Selector.is_unique_path sub_sel then 
          let condition = "k='"^(Selector.to_string sub_sel)^condition_suffix in
          Caqti_driver.get C.conx storage_info.table_name typ ~condition ()
        else
          get_matching_keys storage_info sub_sel
          >|= List.map (fun k -> 
                let condition = "k='"^k^condition_suffix in
                Caqti_driver.get C.conx storage_info.table_name typ ~condition ())
          >>= Apero.LwtM.flatten
          >|= List.flatten
      in
        get_result
        >|= List.map (fun row -> match row with
          | k::v::e::[] ->
            let encoding = Value.encoding_of_string e in
            (match Value.of_string v encoding with 
            | Ok v -> Some (Path.of_string @@ (Path.to_string storage_info.path)^k, v)
            | Error err ->
              let _ = Logs_lwt.warn (fun m -> m "[SQL]: get in KV table %s returned a value for key %s that we failed to transcode: %s"
                        storage_info.table_name k (show_yerror err)) in
              None)
          | _ -> let _ = Logs_lwt.warn (fun m -> m "[SQL]: get in KV table %s returned non k+v+e value: %s" storage_info.table_name (String.concat "," row)) in
              None)
        >|= List.filter Apero.Option.is_some
        >|= List.map (fun o -> Apero.Option.get o)

    let put_kv_table storage_info (selector:Selector.t) (value:Value.t) =
      let%lwt _ = Logs_lwt.debug (fun m -> m "[SQL]: put(%s) into kv table %s"
                    (Selector.to_string selector) (storage_info.table_name)) in
      let open Apero.LwtM.InfixM in
      let sub_sel = Selector.remove_prefix storage_info.path selector in
      let v = Value.to_string value in
      let enc = Value.encoding_to_string @@ Value.encoding value in
      if Selector.is_unique_path sub_sel then
        Caqti_driver.put C.conx storage_info.table_name ("'"^(Selector.to_string sub_sel)^"','"^v^"','"^enc^"'") ()
      else
        let insert_suffix = "','"^v^"','"^enc^"'" in
        get_matching_keys storage_info sub_sel
        >|= List.map (fun k -> 
          Caqti_driver.put C.conx storage_info.table_name ("'"^k^insert_suffix) ())
        >>= Lwt.join
          
    let put_delta_kv_table storage_info selector delta =
      let%lwt _ = Logs_lwt.debug (fun m -> m "[SQL]: put_delta(%s) into kv table %s"
                    (Selector.to_string selector) (storage_info.table_name)) in
      let open Apero.LwtM.InfixM in                                           (* @TODO  DEAL WITH SELECTOR CASE  *)
      get_kv_table storage_info selector
      >>= Lwt_list.iter_p (fun (p,v) -> 
          match Value.update v delta with
          | Ok value ->
              let sub_path = Apero.Astring.after (String.length storage_info.table_name) (Path.to_string p) in
              let v = Value.to_string value in
              let enc = Value.encoding_to_string @@ Value.encoding value in
            Caqti_driver.put C.conx storage_info.table_name ("'"^sub_path^"','"^v^"','"^enc^"'") ()
          | Error e -> Logs_lwt.warn (
            fun m -> m "[SQL]: put_delta on value %s failed: %s" (Value.to_string v) (show_yerror e)))

    let remove_kv_table storage_info selector = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "[SQL]: remove(%s) from kv table %s"
                    (Selector.to_string selector) (storage_info.table_name)) in
      let open Apero.LwtM.InfixM in
      let sub_sel = Selector.remove_prefix storage_info.path selector in
      if Selector.is_unique_path sub_sel then
        Caqti_driver.remove C.conx storage_info.table_name ("k='"^(Selector.to_string sub_sel)^"'") ()
      else
        get_matching_keys storage_info sub_sel
        >|= List.map (fun k -> 
          Caqti_driver.remove C.conx storage_info.table_name ("k='"^k^"'") ())
        >>= Lwt.join




    (*************************)
    (*   Common operations   *)
    (*************************)

    let dispose storage_info =
      match storage_info.on_dispose with
      | Drop -> 
        let _ = Logs_lwt.debug (fun m -> m "[SQL]: dispose storage of table %s dropping it" (storage_info.table_name)) in
        drop_table C.conx storage_info.table_name
      | Truncate ->
        let _ = Logs_lwt.debug (fun m -> m "[SQL]: dispose storage of table %s truncating it" (storage_info.table_name)) in
        trunc_table C.conx storage_info.table_name
      | DoNothing ->
        let _ = Logs_lwt.debug (fun m -> m "[SQL]: dispose storage of table %s keeping it" (storage_info.table_name)) in
        fun () -> Lwt.return_unit 

    let make_kv_table_name () =
      "Yaks_kv_table_"^(Apero.Uuid.make () |> Apero.Uuid.to_string |> String.map (function | '-' -> '_' | c -> c))

    let create_storage ?alias path props =
      let open Apero.LwtM.InfixM in
      let props = Property.Map.union (fun _ _ v2 -> Some v2) C.properties props in
      let table_name = match get_property Key.table props with
        | Some name -> name
        | None -> make_kv_table_name ()
      in
      let on_dispose = get_on_dispose props in
      let _ = Logs_lwt.debug (fun m -> m "[SQL]: create storage for table %s" table_name) in
      let%lwt (schema, is_kv_table) = match%lwt Caqti_driver.get_schema C.conx table_name with
        | Some s -> 
          let is_kv_table = s = Caqti_driver.kv_table_schema in
          let%lwt _ = Logs_lwt.debug (fun m -> m "[SQL]: table %s found (kv table? %b)" table_name is_kv_table) in
          Lwt.return (s, is_kv_table)
        | None -> 
          let%lwt _ = Logs_lwt.debug (fun m -> m "[SQL]: table %s not found - create it as key/value table" table_name) in
          Caqti_driver.create_kv_table C.conx table_name props >>= fun r -> Lwt.return (r, true)
      in
      let storage_info = { path; props; table_name; schema; on_dispose } in
      if is_kv_table then
        Lwt.return @@
        Storage.make ?alias path props
          (dispose storage_info)
          (get_kv_table storage_info)
          (put_kv_table storage_info)
          (put_delta_kv_table storage_info)
          (remove_kv_table storage_info)
      else
        Lwt.return @@
        Storage.make ?alias path props
          (dispose storage_info)
          (get_sql_table storage_info)
          (put_sql_table storage_info)
          (put_delta_sql_table storage_info)
          (remove_sql_table storage_info)
  end
end 

let make_sql_be props =
  let url = match get_property Be_sql_property.Key.url props with
    | Some url -> url
    | None -> raise @@ YException (`InvalidBackendProperty (`Msg ("Property "^Be_sql_property.Key.url^" is not specified")))
  in
  let connection = Caqti_driver.connect url in
  let module M = SQLBE.Make (
    struct 
      let id = Apero.Uuid.make ()
      let properties = add_property Property.Backend.Key.kind Property.Backend.Value.dbms props
      let conx = connection
    end) (Apero.MVar_lwt)
  in (module M : Backend)

module SQLBEF = struct 
  let make (props:properties) = make_sql_be props
  let name = Property.Backend.Value.memory
end

