open Yaks_types
open Yaks_core_types



let remote_query_handler promise mlist decoder sample =
  Guard.guarded mlist
  @@ fun xs ->
  match sample with
  | Zenoh.StorageData {stoid; rsn=_; resname; data; info=_} ->
    let store_id = Abuf.hexdump ~separator:":" stoid in
    let _ = Logs.debug (fun m -> m ">>> Query Handler Received data for key: %s from storage %s" resname store_id) in
    (try decoder data |> fun value -> 
        Logs.debug (fun m -> m ">>> Query Handler parsed data for key: %s" resname);
        Guard.return () ((store_id, Path.of_string(resname), value)::xs)
    with e -> 
        Logs.err (fun m -> m ">>> Query Handler failed to parse data for key %s : %s" resname (Printexc.to_string e));
        Guard.return () xs)

    (* (match
    let _ = Logs.debug (fun m -> m ">>> Query Handler Received data for key: %s" resname) in
    let store_id = Abuf.hexdump ~separator:":" stoid in
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
    | _ -> Guard.return () xs) *)

  | Zenoh.StorageFinal {stoid; rsn;} ->
      let store_id = Abuf.hexdump ~separator:":" stoid in
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
  let%lwt () = Zenoh.query zenoh path query (remote_query_handler r mlist decoder) in
  p >>= Lwt_list.map_p (fun (_,p,v) -> Lwt.return (p, v))     (* Note:  Zenoh StorageIds is ignored (no need) *)

let write path value zenoh =
  let res = Path.to_string path in
  let buf = Abuf.create ~grow:8192 8192 in 
  TimedValue.encode value buf;
  Zenoh.write zenoh res buf 

let subscribe zenoh selector is_push notify_call =
  let open Lwt.Infix in
  let sub_mode = if is_push then Zenoh.push_mode else Zenoh.pull_mode in
  let listener path datas = 
    Lwt_list.iter_s (fun (buf, _) ->
        Lwt.catch (fun () -> TimedValue.decode buf |> Lwt.return) 
                (fun e -> Logs_lwt.warn (fun m -> m "Error while decoding value received for Zenoh subscription: \n%s" (Printexc.to_string e)) >>= fun () -> Lwt.fail e) >>= fun tv ->
        notify_call [(Path.of_string path, tv.value)]) datas
  in
  Zenoh.subscribe zenoh (Selector.to_string selector) listener ~mode:sub_mode
