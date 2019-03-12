open Apero
open Yaks_types
open Yaks_core_types
open Yaks_core_properties

module Storage = struct

  module Id = Apero.Uuid
  module ZUtils = Yaks_zenoh_utils

  type t = 
    { id : Id.t
    ; selector : Selector.t
    ; props : properties
    ; hlc : HLC.t
    ; dispose : unit -> unit Lwt.t
    ; get : Selector.t -> (Path.t * TimedValue.t) list Lwt.t
    ; put : Path.t -> TimedValue.t -> unit Lwt.t 
    ; put_delta : Path.t -> TimedValue.t -> unit Lwt.t 
    ; remove : Path.t -> Timestamp.t -> unit Lwt.t
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


  let on_zenoh_write s (resid:string) (samples:(Abuf.t * Ztypes.data_info) list) =
    let open Lwt.Infix in
    let check_time_validity time =
      match%lwt HLC.update_with_timestamp time s.hlc with
      | Ok () -> Lwt.return_true
      | Error e -> let _ = Logs_lwt.warn (fun m -> m "[Sto] %s: Remote change for key %s refused: timestamp differs too much from local clock: %s" (Id.to_string s.id) resid (Apero.show_error e)) in Lwt.return_false
    in
    match Path.of_string_opt resid with
    | Some path ->
      ZUtils.decode_changes s.hlc samples >>=
      Lwt_list.iter_s (function
        | Put(tv)      -> if%lwt check_time_validity tv.time then put s path tv
        | Update(tv)   -> if%lwt check_time_validity tv.time then update s path tv
        | Remove(time) -> if%lwt check_time_validity time then remove s path time
      )
    | None -> let _ = Logs_lwt.err (fun m -> m "[Sto] %s: INTERNAL ERROR receiving data via Zenoh for an invalid path: %s" (Id.to_string s.id) resid) in Lwt.return_unit

  let on_zenoh_query s resname predicate = 
    let sel = if predicate = "" then resname else resname ^"?"^predicate in 
    let%lwt _ = Logs_lwt.debug (fun m -> m "[Sto] %s: Handling remote query on storage for %s?%s" (Id.to_string s.id) resname predicate) in 
    match Selector.of_string_opt sel with 
    | Some selector ->
      let%lwt (kvs:(Path.t*TimedValue.t) list) = get s selector in
      let evs = List.map
        (fun ((path,tv):(Path.t*TimedValue.t)) ->
          let spath = Path.to_string path in
          let encoding = Some(ZUtils.encoding_to_flag tv.value) in
          let data_info = { Ztypes.empty_data_info with encoding; ts=Some(tv.time) } in
          let buf = Abuf.create ~grow:8192 8192 in
          ZUtils.encode_value tv.value buf;
          (spath, buf, data_info)) kvs
      in
      Lwt.return evs 
    | _ -> 
      let%lwt _ = Logs_lwt.debug (fun m -> m "[Sto] %s: Unable to resolve query for %s - not a Selector" (Id.to_string s.id) sel) in 
      Lwt.return []


  let align s zenoh selector =
    let%lwt _ = Logs_lwt.debug (fun m -> m "[Sto] %s: align with remote storages..." (Id.to_string s.id)) in
    let check_time t path = match%lwt HLC.update_with_timestamp t s.hlc with
      | Ok () -> Lwt.return_true 
      | Error e -> let _ = Logs_lwt.warn (fun m -> m "[Sto] %s: align refuses update for key %s: timestamp differs too much from local clock: %s"
        (Id.to_string s.id) (Path.to_string path) (Apero.show_error e)) in Lwt.return_false
    in

    (* create a temporary Zenoh listener (to not miss ongoing updates) *)
    let listener path (changes:change list) =
      Lwt_list.iter_s (fun change -> 
        if Astring.is_prefix ~affix:"/@" (Path.to_string path) then Lwt.return_unit
        else match change with
        | Put tv -> if%lwt check_time tv.time path then put s path tv
        | Update tv -> if%lwt check_time tv.time path then update s path tv
        | Remove time -> if%lwt check_time time path then remove s path time) changes
    in
    let%lwt tmp_sub = ZUtils.subscribe zenoh s.hlc selector true listener in

    (* query the remote storages (to get historical data) *)
    let%lwt kvs = ZUtils.query zenoh  s.hlc selector in
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
    Lwt.async (fun() -> Lwt_unix.sleep 10.0 >> Zenoh.unsubscribe zenoh tmp_sub);
    Lwt.return_unit

end  [@@deriving show]

