open Apero
open Yaks_types
open Yaks_core_types



let remote_query_handler promise mlist decoder sample =
  Guard.guarded mlist
  @@ fun xs ->
  match sample with
  | Zenoh.StorageData {stoid; rsn=_; resname; data} ->
    (match
    let _ = Logs.debug (fun m -> m ">>> Query Handler Received data for key: %s" resname) in
    let store_id = IOBuf.hexdump ~separator:":" stoid in
    let _ = Logs.debug (fun m -> m ">>> Query Handler Received data for key: %s from storage %s" resname store_id) in
    match decoder data with
    | Ok (value, _) ->
        let _ = Logs.debug (fun m -> m ">>> Query Handler parsed data for key: %s" resname) in
        Ok(store_id, Path.of_string(resname), value)
    | Error e ->
        let _ = Logs.err (fun m -> m ">>> Query Handler failed to parse data for key %s : %s" resname (Atypes.show_error e)) in
        Error e
    with
    | Ok sample -> Guard.return () (sample::xs)
    | _ -> Guard.return () xs)
  | Zenoh.StorageFinal {stoid; rsn;} ->
      let store_id = IOBuf.hexdump ~separator:":" stoid in
      let%lwt _ = Logs_lwt.debug (fun m -> m "QUERY HANDLER RECIEVED FROM STORAGE [%-16s:%02i] FINAL\n%!" (store_id) rsn) in
      Guard.return () xs
  | Zenoh.ReplyFinal ->
      let%lwt _ = Logs_lwt.debug (fun m -> m "QUERY HANDLER RECIEVED GLOBAL FINAL\n%!") in
      Lwt.wakeup_later promise xs;
      Guard.return () xs


let query zenoh selector decoder =
  let open Lwt.Infix in
  let path = Selector.path selector in
  let p,r = Lwt.wait () in
  let mlist = Guard.create [] in
  let query = Selector.optional_part selector in
  let _ = Logs_lwt.debug (fun m -> m "[Yeng]: Send Zenoh query for '%s' '%s'" path query) in
  let%lwt () = Zenoh.query path query (remote_query_handler r mlist decoder) zenoh in
  p >>= Lwt_list.map_p (fun (_,p,v) -> Lwt.return (p, v))     (* Note:  Zenoh StorageIds is ignored (no need) *)

let write path value zenoh =
  let res = Path.to_string path in
  let buf = IOBuf.create ~grow:8192 8192 in 
  match (TimedValue.encode value buf) with 
  | Ok buf -> 
    let buf' = IOBuf.flip buf in 
    Zenoh.write buf' res zenoh 
  | Error e -> Lwt.fail @@ Exception e 

let subscribe zenoh selector is_push notify_call =
  let sub_mode = if is_push then Zenoh.push_mode else Zenoh.pull_mode in
  let listener buf path = 
    match TimedValue.decode buf with 
    | Ok (tv, _) ->
      notify_call [(Path.of_string path, tv.value)]
    | Error e ->
      let%lwt _ = Logs_lwt.warn (fun m -> m "Error while decoding value received for Zenoh subscription: \n%s" @@ Atypes.show_error e) 
      in Lwt.return_unit
  in
  Zenoh.subscribe (Selector.to_string selector) listener ~mode:sub_mode zenoh
