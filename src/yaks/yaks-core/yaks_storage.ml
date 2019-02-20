open Apero
open Yaks_types
open Yaks_core_types
open Yaks_core_properties

module Storage = struct

  module Id = Apero.Uuid

  type t = 
    { id : Id.t
    ; selector : Selector.t
    ; props : properties
    ; hlc : HLC.t
    ; dispose : unit -> unit Lwt.t
    ; get : Selector.t -> (Path.t * TimedValue.t) list Lwt.t
    ; put : Path.t -> TimedValue.t -> unit Lwt.t 
    ; put_delta : Path.t -> TimedValue.t -> unit Lwt.t 
    ; remove : Path.t -> unit Lwt.t
    ; as_string : string
    }

  let make selector props hlc dispose get put put_delta remove =
    let alias = Properties.get Property.Storage.Key.alias props in   
    let uuid = match alias with | Some(a) -> Id.make_from_alias a | None -> Id.make () in
    { id = uuid; selector; props; hlc; dispose; get; put; put_delta; remove;
      as_string = "Sto#"^(Id.to_string uuid)^"("^(Selector.to_string selector)^")"
    }

  let dispose s = s.dispose ()
  let id s = s.id
  let alias s = Apero.Uuid.alias s.id
  let selector s = s.selector
  let properties s = s.props

  let to_string s = s.as_string

  let covers_fully s sel = Selector.includes ~subsel:sel s.selector
  let covers_partially s sel = Selector.intersects sel s.selector

  let get s = s.get
  let put s = s.put
  let update s = s.put_delta
  let remove s = s.remove


  let on_zenoh_write s (sample:Abuf.t) (key:string) =
    let%lwt _ = Logs_lwt.debug (fun m -> m "[Sto] %s: Received remote update for key %s" (Id.to_string s.id) key) in
    match Path.of_string_opt key with 
    | Some path ->
      (try TimedValue.decode sample |> fun v -> 
          match%lwt HLC.update_with_timestamp v.time s.hlc with
          | Ok () -> put s path v
          | Error e -> Logs_lwt.warn (fun m -> m "[Sto] %s: Remote update for key %s refused: timestamp differs too much from local clock: %s" (Id.to_string s.id) key (Apero.show_error e))
      with e -> Logs_lwt.debug (fun m -> m "[Sto] %s: Failed to decode TimedValue.t to be stored for key %s: %s" (Id.to_string s.id) key (Printexc.to_string e)))
    | None -> 
      Logs_lwt.warn (fun m -> m "[Sto] %s: Received data via Zenoh for key %s which is not a path" (Id.to_string s.id) key) 


  let on_zenoh_query s resname predicate = 
    let sel = if predicate = "" then resname else resname ^"?"^predicate in 
    let%lwt _ = Logs_lwt.debug (fun m -> m "[Sto] %s: Handling remote query on storage for %s?%s" (Id.to_string s.id) resname predicate) in 
    match Selector.of_string_opt sel with 
    | Some selector ->
      let%lwt kvs = get s selector in
      let evs = List.map
        (fun (path,value) ->
          let spath = Path.to_string path in
          let buf = Abuf.create ~grow:4096 4096 in 
          TimedValue.encode value buf;
          (spath, buf)) kvs in
      Lwt.return evs 
    | _ -> 
      let%lwt _ = Logs_lwt.debug (fun m -> m "[Sto] %s: Unable to resolve query for %s - not a Selector" (Id.to_string s.id) sel) in 
      Lwt.return []

  let align s zenoh selector =
    let%lwt _ = Logs_lwt.debug (fun m -> m "[Sto] %s: align with remote storages..." (Id.to_string s.id)) in
    (* create a temporary Zenoh listener (to not miss ongoing updates) *)
    let listener buf path =
      if Astring.is_prefix ~affix:"/@" path then Lwt.return_unit
      else 
      (try TimedValue.decode buf |> Result.return with e -> Error e)|> function 
      | Ok tv ->
        (match%lwt HLC.update_with_timestamp tv.time s.hlc with
        | Ok () -> put s (Path.of_string path) tv
        | Error e -> Logs_lwt.warn (fun m -> m "[Sto] %s: align refuses update for key %s: timestamp differs too much from local clock: %s" (Id.to_string s.id) path (Apero.show_error e)))
      | Error e -> Logs_lwt.warn (fun m -> m "[Sto] %s: Error while decoding value received for alignment: \n%s"  (Id.to_string s.id) (Printexc.to_string e))
    in
    let%lwt tmp_sub = Zenoh.subscribe (Selector.to_string selector) listener ~mode:Zenoh.push_mode zenoh in
    (* query the remote storages (to get historical data) *)
    let%lwt kvs = Yaks_zenoh_utils.query zenoh selector TimedValue.decode in
    let%lwt () = List.map (fun (path, (tv:TimedValue.t)) ->
      if Astring.is_prefix ~affix:"/@" (Path.to_string path) then Lwt.return_unit
      else
        (match%lwt HLC.update_with_timestamp tv.time s.hlc with
        | Ok () -> put s path tv
        | Error e -> 
          Logs_lwt.warn (fun m -> m "[Sto] %s: align refuses update for key %s: timestamp differs too much from local clock: %s" (Id.to_string s.id) (Path.to_string path) (Apero.show_error e)))
      )
      kvs
      |> Lwt.join
    in
    let%lwt _ = Logs_lwt.debug (fun m -> m "[Sto] %s: alignment done" (Id.to_string s.id)) in
    let open Apero.LwtM.InfixM in
    (* program the removal of the temporary Zenoh listener after a while *)
    Lwt.async (fun() -> Lwt_unix.sleep 10.0 >> Zenoh.unsubscribe tmp_sub zenoh);
    Lwt.return_unit

end  [@@deriving show]

