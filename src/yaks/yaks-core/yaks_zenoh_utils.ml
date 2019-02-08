open Apero
open Yaks_types
open Yaks_core_types

module Make (MVar: Apero.MVar) = struct

  let remote_query_handler promise mlist decoder sample =
    MVar.guarded mlist
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
      | Ok sample -> MVar.return () (sample::xs)
      | _ -> MVar.return () xs)
    | Zenoh.StorageFinal {stoid; rsn;} ->
        let store_id = IOBuf.hexdump ~separator:":" stoid in
        let%lwt _ = Logs_lwt.debug (fun m -> m "QUERY HANDLER RECIEVED FROM STORAGE [%-16s:%02i] FINAL\n%!" (store_id) rsn) in
        MVar.return () xs
    | Zenoh.ReplyFinal ->
        let%lwt _ = Logs_lwt.debug (fun m -> m "QUERY HANDLER RECIEVED GLOBAL FINAL\n%!") in
        Lwt.wakeup_later promise xs;
        MVar.return () xs


  let issue_remote_query zenoh selector decoder =
    let path = Selector.path selector in
    let p,r = Lwt.wait () in
    let mlist = MVar.create [] in
    let query = Selector.optional_part selector in
    let _ = Logs_lwt.debug (fun m -> m "[Yeng]: Send Zenoh query for '%s' '%s'" path query) in
    let%lwt () = Zenoh.query path query (remote_query_handler r mlist decoder) zenoh in
    let open Lwt.Infix in
    p >>= Lwt_list.map_p (fun (_,p,v) -> Lwt.return (p, v))     (* Note:  Zenoh StorageIds is ignored (no need) *)

    let distribute_update path value zenoh =
      let res = Path.to_string path in
      let buf = IOBuf.create ~grow:8192 8192 in 
      match (TimedValue.encode value buf) with 
      | Ok buf -> 
        let buf' = IOBuf.flip buf in 
        Zenoh.write buf' res zenoh 
      | Error e -> Lwt.fail @@ Exception e 

end