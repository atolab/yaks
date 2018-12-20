open Apero
open Yaks_core
open Caqti_driver
open Be_sql_property

module SQLBE = struct 

  module type Config = sig    
    val id : BeId.t 
    val properties : properties
    val conx : connection
  end

  module Make (C : Config) (MVar : Apero.MVar) = struct 

    let id = C.id
    let properties = C.properties

    let to_string = "SQLBE#"^(BeId.to_string C.id)^"{"^(Properties.to_string properties)^"}"

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
      if Selector.is_matching_path storage_info.path selector then
        let%lwt _ = Logs_lwt.debug (fun m -> m "[SQL]: get(%s) from legacy table %s"
                      (Selector.to_string selector) (storage_info.table_name)) in
        let open Lwt.Infix in
        let (col_names, typ) = storage_info.schema in
        Caqti_driver.get C.conx storage_info.table_name typ ?condition:(Selector.predicate selector) ()
        >|= List.map (fun row -> storage_info.path, Value.SqlValue (row, Some col_names)) 
      else
        let%lwt _ = Logs_lwt.warn (fun m -> m "[SQL]: Can't get Selector %s in Storage with path %s - the exact Storage path is required"
                      (Selector.to_string selector) (Path.to_string storage_info.path))
        in Lwt.return []

    let put_sql_table storage_info (path:Path.t) (value:Value.t) =
      if Path.is_prefix ~affix:storage_info.path path then
        let%lwt _ = Logs_lwt.debug (fun m -> m "[SQL]: put(%s) into legacy table %s"
                      (Path.to_string path) (storage_info.table_name)) in
        let open Value in 
        match transcode value Sql_Encoding with 
        | Ok SqlValue (row, _) -> Caqti_driver.put C.conx storage_info.table_name storage_info.schema row ()
        | Ok _ -> Lwt.fail @@ YException (`UnsupportedTranscoding (`Msg "Transcoding to SQL didn't return an SqlValue"))
        | Error e -> Lwt.fail @@ YException e
      else
        let%lwt _ = Logs_lwt.warn (fun m -> m "[SQL]: Can't put Path %s in Storage with path %s - the exact Storage path is required"
                                      (Path.to_string path) (Path.to_string storage_info.path))
        in Lwt.return_unit

    let update_sql_table storage_info path delta =
      let open Apero.LwtM.InfixM in
      if Path.is_prefix ~affix:storage_info.path path then
        let%lwt _ = Logs_lwt.debug (fun m -> m "[SQL]: put_delta(%s) into legacy table %s"
                      (Path.to_string path) (storage_info.table_name)) in
        get_sql_table storage_info (Selector.of_path path)
        >>= Lwt_list.iter_p (fun (_,v) -> 
            match Value.update v delta with
            | Ok SqlValue (row, _) ->
              Caqti_driver.put C.conx storage_info.table_name storage_info.schema row ()
            | Ok _ -> Logs_lwt.warn (
              fun m -> m "[SQL]: put_delta on value %s failed: update of SqlValue didn't return a SqlValue" (Value.to_string v))
            | Error e -> Logs_lwt.warn (
              fun m -> m "[SQL]: put_delta on value %s failed: %s" (Value.to_string v) (show_yerror e)))
      else
        let%lwt _ = Logs_lwt.warn (fun m -> m "[SQL]: Can't put_delta Path %s in Storage with path %s - the exact Storage path is required"
                                      (Path.to_string path) (Path.to_string storage_info.path))
        in Lwt.return_unit

    let remove_sql_table storage_info path = 
      if Path.is_prefix ~affix:storage_info.path path then
        (* TODO:
           Remove operation used to be on Selectors with a query (see commented code below).
           But now that a Path is given as argument, there is no longer a query part, and thus
           we can't decide which ro to remove in the SQL table.
           What we need is really to expose the SQL keys of the table in the Yaks path (similarly to KV tables below) 
         *)
        (*
        let%lwt _ = Logs_lwt.debug (fun m -> m "[SQL]: remove(%s) from legacy table %s"
                      (Path.to_string path) (storage_info.table_name)) in
        match Path.get_query path with
        | Some q -> Caqti_driver.remove C.conx storage_info.table_name q ()
        | None -> 
          let _ = Logs_lwt.debug (fun m -> m "[SQL]: Can't remove path %s without a query" (Path.to_string path)) in
          Lwt.return_unit
        *)
        let%lwt _ = Logs_lwt.err (fun m -> m "[SQL]: Can't remove Path %s in Storage with path %s - remove operation for legacy SQL tables is not implemented"
                                      (Path.to_string path) (Path.to_string storage_info.path))
        in Lwt.return_unit
      else
        let%lwt _ = Logs_lwt.warn (fun m -> m "[SQL]: Can't remove Path %s in Storage with path %s - the exact Storage path is required"
                                      (Path.to_string path) (Path.to_string storage_info.path))
        in Lwt.return_unit



    (***************************************)
    (*   Operations on key/value  tables   *)
    (***************************************)
    let to_sql_string s = "'"^s^"'"

    let get_kv_condition sub_selector = 
      let sql_selector = Selector.path sub_selector
        |> Apero.Astring.replace '*' '%'
      in
        match Selector.predicate sub_selector with
        | Some q -> "k like '"^sql_selector^"' AND "^q
        | None   -> "k like '"^sql_selector^"'"

    let get_matching_keys storage_info sub_selector =
      let open Apero.LwtM.InfixM in
      let condition = get_kv_condition sub_selector in
      Caqti_driver.get_keys_kv_table C.conx storage_info.table_name ~condition
          >|= List.filter (fun k -> Selector.is_matching_path (Path.of_string k) sub_selector)
          >|= fun l -> let _ = Logs_lwt.debug (fun m -> m "[SQL]: in %s found matching keys of %s : %s"
                    (storage_info.table_name) (Selector.to_string sub_selector) (String.concat " " l)) in l

    let get_kv_table storage_info (selector:Selector.t) =
      let%lwt _ = Logs_lwt.debug (fun m -> m "[SQL]: get(%s) from kv table %s"
                    (Selector.to_string selector) (storage_info.table_name)) in
      let open Apero.LwtM.InfixM in
      let (_, typ) = storage_info.schema in
      match Selector.remove_matching_prefix storage_info.path selector with
      | None -> Lwt.return []
      | Some sub_sel ->
        let condition = get_kv_condition sub_sel in
          Caqti_driver.get C.conx storage_info.table_name typ ~condition ()
          (* NOTE: replacing '*' with '%' in LIKE clause gives more keys than we want (as % is similar to %%). We need to filter: *)
          >|= List.filter (fun row -> match row with
            | k::_::_::[] -> Selector.is_matching_path (Path.of_string k) sub_sel
            | _ -> let _ = Logs_lwt.warn (fun m -> m "[SQL]: get in KV table %s returned non k+v+e value: %s" storage_info.table_name (String.concat "," row)) in false
            )
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

    let put_kv_table storage_info (path:Path.t) (value:Value.t) =
      let%lwt _ = Logs_lwt.debug (fun m -> m "[SQL]: put(%s) into kv table %s"
                    (Path.to_string path) (storage_info.table_name)) in
      let sub_path = Path.remove_prefix (Path.length storage_info.path) path in
      let v = Value.to_string value |> to_sql_string in
      let enc = Value.encoding_to_string @@ Value.encoding value |> to_sql_string in
      Caqti_driver.put C.conx storage_info.table_name storage_info.schema ((Path.to_string sub_path |> to_sql_string)::v::enc::[]) ()

    let update_kv_table storage_info path delta =
      let%lwt _ = Logs_lwt.debug (fun m -> m "[SQL]: put_delta(%s) into kv table %s"
                    (Path.to_string path) (storage_info.table_name)) in
      let open Apero.LwtM.InfixM in
      get_kv_table storage_info (Selector.of_path path)
      >>= Lwt_list.iter_p (fun (p,v) -> 
          match Value.update v delta with
          | Ok value ->
              let sub_path = Apero.Astring.after (String.length storage_info.table_name) (Path.to_string p) |> to_sql_string in
              let v = Value.to_string value |> to_sql_string in
              let enc = Value.encoding_to_string @@ Value.encoding value |> to_sql_string in
            Caqti_driver.put C.conx storage_info.table_name storage_info.schema (sub_path::v::enc::[]) ()
          | Error e -> Logs_lwt.warn (
            fun m -> m "[SQL]: put_delta on value %s failed: %s" (Value.to_string v) (show_yerror e)))

    let remove_kv_table storage_info path = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "[SQL]: remove(%s) from kv table %s"
                    (Path.to_string path) (storage_info.table_name)) in
      let sub_path = Path.remove_prefix (Path.length storage_info.path) path in
      Caqti_driver.remove C.conx storage_info.table_name ("k='"^(Path.to_string sub_path)^"'") ()



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

    let create_storage path props =
      let open Apero.LwtM.InfixM in
      let props = Properties.union (fun _ _ v2 -> Some v2) C.properties props in
      let table_name = match Properties.get Key.table props with
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
        Storage.make path props
          (dispose storage_info)
          (get_kv_table storage_info)
          (put_kv_table storage_info)
          (update_kv_table storage_info)
          (remove_kv_table storage_info)
      else
        Lwt.return @@
        Storage.make path props
          (dispose storage_info)
          (get_sql_table storage_info)
          (put_sql_table storage_info)
          (update_sql_table storage_info)
          (remove_sql_table storage_info)
  end
end 


module SQLBEF = struct 
  let kind = Property.Backend.Value.memory

  let make id properties =
    let url = match Properties.get Be_sql_property.Key.url properties with
      | Some url -> url
      | None -> raise @@ YException (`InvalidBackendProperty (`Msg ("Property "^Be_sql_property.Key.url^" is not specified")))
    in
    let connection = Caqti_driver.connect url in
    let module M = SQLBE.Make (
      struct 
        let id = id
        let properties = Properties.add Property.Backend.Key.kind Property.Backend.Value.dbms properties
        let conx = connection
      end) (Apero.MVar_lwt)
    in (module M : Backend)

end

