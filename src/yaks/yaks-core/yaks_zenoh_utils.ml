open Apero
open Yaks_types
open Yaks_common_errors

let kind_put = 0L
let kind_update = 1L
let kind_remove = 2L

let encoding_to_flag v = Value.encoding v |> Value.encoding_to_int |> Int64.of_int
let encoding_of_flag f = match f with 
  | None -> Value.RAW 
  | Some i -> Option.get_or_default (Value.int_to_encoding (Int64.to_int i)) Value.RAW

let encode_value v buf = match Value.transcode v Value.RAW with
  | Ok Value.RawValue(_, b) -> Apero.encode_bytes b buf
  | Ok v' -> Logs.err (fun m -> m "[Yeng]: INTERNAL ERROR: transcode of value '%s' to RAW didn't return a RawValue but: '%s'" (Value.to_string v) (Value.to_string v'))
  | Error e -> Logs.err (fun m -> m "[Yeng]: INTERNAL ERROR: transcode of value '%s' to RAW failed: '%s'" (Value.to_string v) (show_yerror e))
let decode_value buf encoding =
  let raw_value = Value.RawValue(None, Apero.decode_bytes buf) in
  match Value.transcode raw_value encoding with
  | Ok v -> v
  | Error e -> raise @@ YException e

let decode_time hlc (info:Ztypes.data_info) = match info.ts with 
  | Some t -> Lwt.return t 
  | None -> Logs.warn (fun m -> m "Received a data from Zenoh without timestamp; generate it");
    HLC.new_timestamp hlc

let decode_change hlc (buf:Abuf.t) (info:Ztypes.data_info) =
  let%lwt time = decode_time hlc info in
  let encoding = encoding_of_flag info.encoding in
  let k = Option.get_or_default info.kind kind_put in
  if k=kind_put then
    Lwt.return @@ Put {time; value=decode_value buf encoding}
  else if k=kind_update then
    Lwt.return @@ Update {time; value=decode_value buf encoding}
  else if k=kind_remove then
    Lwt.return @@ Remove time 
  else
    Lwt.fail @@ YException (`InternalError (`Msg ("Unkown kind value in incoming Zenoh message: "^(Int64.to_string k))))

let decode_changes hlc samples =
  List.map (fun (buf, (info:Ztypes.data_info)) -> decode_change hlc buf info) samples |>
  Lwt_list.fold_left_s (fun acc lwt ->
    (* drop the failing Lwt.t (e.g. decoding failure), logging an error for each *)
    Lwt.try_bind (fun () -> lwt) (fun x -> Lwt.return @@ x::acc)
    (fun e -> Logs.err (fun m -> m "[Yeng]: INTERNAL ERROR receiving data via Zenoh: %s" (Printexc.to_string e)); Lwt.return acc)
  ) []


let remote_query_handler promise mlist hlc sample =
  Guard.guarded mlist
  @@ fun xs ->
  match sample with
  | Zenoh.StorageData {stoid; rsn=_; resname; data; info} ->
    let store_id = Abuf.hexdump ~separator:":" stoid in
    Logs.debug (fun m -> m ">>> Query Handler Received data for key: %s from storage %s" resname store_id);
    let%lwt time = decode_time hlc info in
    let encoding = encoding_of_flag info.encoding in
    (try decode_value data encoding |> fun value -> 
        Logs.debug (fun m -> m ">>> Query Handler parsed data for key: %s" resname);
        let tv = TimedValue.{time; value} in
        Guard.return () ((store_id, Path.of_string(resname), tv)::xs)
    with e -> 
        Logs.err (fun m -> m ">>> Query Handler failed to parse data for key %s : %s \n %s" resname (Printexc.to_string e) (Printexc.get_backtrace ()));
        Guard.return () xs)
  | Zenoh.StorageFinal {stoid; rsn;} ->
      let store_id = Abuf.hexdump ~separator:":" stoid in
      Logs.debug (fun m -> m "QUERY HANDLER RECIEVED FROM STORAGE [%-16s:%02i] FINAL\n%!" (store_id) rsn);
      Guard.return () xs
  | Zenoh.ReplyFinal ->
      Logs.debug (fun m -> m "QUERY HANDLER RECIEVED GLOBAL FINAL\n%!");
      Lwt.wakeup_later promise xs;
      Guard.return () xs


let query zenoh hlc selector =
  let open Lwt.Infix in
  let path = Selector.path selector in
  let p,r = Lwt.wait () in
  let mlist = Guard.create [] in
  let query = Selector.optional_part selector in
  Logs.debug (fun m -> m "[Yeng]: Send Zenoh query for '%s' '%s'" path query);
  let%lwt () = Zenoh.query zenoh path query (remote_query_handler r mlist hlc) in
  p >>= Lwt_list.map_p (fun (_,p,v) -> Lwt.return (p, v))     (* Note:  Zenoh StorageIds is ignored (no need) *)

let subscribe zenoh hlc selector is_push notify_call =
  let sub_mode = if is_push then Zenoh.push_mode else Zenoh.pull_mode in
  let listener resid samples =
    match Path.of_string_opt resid with
    | Some path -> let%lwt changes = decode_changes hlc samples in notify_call path changes
    | None -> Logs.err (fun m -> m "[Yeng]: INTERNAL ERROR receiving data via Zenoh for an invalid path: %s" resid); Lwt.return_unit
  in
  Zenoh.subscribe zenoh (Selector.to_string selector) listener ~mode:sub_mode

let unsubscribe = Zenoh.unsubscribe


let store zenoh hlc selector (sublistener:Path.t -> change list -> unit Lwt.t) (query_handler:Selector.t -> (Path.t * TimedValue.t) list Lwt.t) =
  let zsublistener (resname:string) (samples:(Abuf.t * Ztypes.data_info) list) =
    let open Lwt.Infix in
    match Path.of_string_opt resname with
    | Some path -> decode_changes hlc samples >>= sublistener path
    | None -> Logs.warn (fun m -> m "[Zenh]: Store received data for resource %s which is not a valid path" resname); Lwt.return_unit
  in
  let zquery_handler resname predicate =
    let s = if predicate = "" then resname else resname ^"?"^predicate in
    match Selector.of_string_opt s with
    | Some selector ->
      let%lwt kvs = query_handler selector in
      Lwt.return @@ List.map (fun ((path,tv):(Path.t*TimedValue.t)) ->
        let spath = Path.to_string path in
        let encoding = Some(encoding_to_flag tv.value) in
        let data_info = { Ztypes.empty_data_info with encoding; ts=Some(tv.time) } in
        let buf = Abuf.create ~grow:8192 8192 in
        encode_value tv.value buf;
        (spath, buf, data_info)) kvs
    | None -> Logs.warn (fun m -> m "[Zenh]: Store received query for resource/predicate %s?%s which is not a valid selector" resname predicate); Lwt.return []

  in
  Zenoh.store zenoh (Selector.to_string selector) zsublistener zquery_handler

let unstore = Zenoh.unstore



let send_put path (tv:TimedValue.t) zenoh =
  let res = Path.to_string path in
  let buf = Abuf.create ~grow:8192 8192 in
  let encoding = encoding_to_flag tv.value in
  encode_value tv.value buf;
  Zenoh.write zenoh res ~timestamp:tv.time ~encoding buf

let send_update path (tv:TimedValue.t) zenoh =
  let res = Path.to_string path in
  let buf = Abuf.create ~grow:8192 8192 in
  let encoding = encoding_to_flag tv.value in
  encode_value tv.value buf;
  Zenoh.write zenoh res ~timestamp:tv.time ~encoding ~kind:kind_update buf

let empty_buf = Abuf.create 0
let send_remove path timestamp zenoh =
  let res = Path.to_string path in
  Zenoh.write zenoh res ~timestamp ~kind:kind_remove empty_buf

