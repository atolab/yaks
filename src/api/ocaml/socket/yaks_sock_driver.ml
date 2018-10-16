open Lwt.Infix

module MVar = Apero.MVar_lwt


module WorkingMap = Map.Make(Apero.Vle)
module ListenersMap = Map.Make(Yaks_sock_types.SubscriberId) 

type state = {
  sock : Lwt_unix.file_descr
; working_set : Yaks_fe_sock_types.message Lwt.u WorkingMap.t
; subscribers : Yaks_sock_types.listener ListenersMap.t
}

type t = state MVar.t 

let max_size = 64 * 1024



let decode_body (mid:Yaks_fe_sock_codes.message_id) (_:char) (buf: Apero.IOBuf.t) = 
  let open Apero in
  let open Yaks_fe_sock_codec in
  let open Yaks_fe_sock_codes in
  let open Yaks_fe_sock_types in
  let open Apero.Result.Infix in
  match mid with 
  | OK -> 
    ignore @@ Logs_lwt.debug (fun m -> m "[YAS] Received Ok");
    Result.ok (YEmpty, buf)
  | VALUES -> 
    ignore @@ Logs_lwt.debug (fun m -> m "[YAS] Received Values");
    let decode_sv = decode_pair decode_selector decode_value in 
    let decode_svs = decode_seq decode_sv in 
    decode_svs buf
    >>= fun (svs, buf) -> Result.ok (YSelectorValueList svs, buf)
  | NOTIFY ->
    ignore @@ Logs_lwt.debug (fun m -> m "[YAS] Received Notify");
    let decode_sv = decode_pair decode_paths decode_value in 
    let decode_svs = decode_seq decode_sv in
    decode_string buf >>= fun (sid, buf) -> decode_svs buf
    >>= fun (svs, buf) ->
    Result.ok (YNotification (sid, svs), buf)
  | ERROR -> decode_vle buf >>= fun (errno, buf) -> 
    ignore @@ Logs_lwt.warn (fun m -> m "[YAS] Received ErrNo : %d" @@ Int64.to_int errno);
    Result.ok (YErrorInfo errno, buf)
  (* These are the messages that the client send but do not expect to receive. 
     If any of this message is received the server is considered malfunctioning or 
     malicious and the connection is immediately closed *)
  | OPEN | VALUE | DELETE | PUT | PATCH | GET | SUB | UNSUB | EVAL | CREATE -> 
    let _ = Logs_lwt.warn (fun m -> m "[FES] Unexpexted Message") in
    Result.fail `UnexpextedMessage 

let decode_message buf = 
  let open Apero.Result.Infix in
  let open Yaks_fe_sock_types in
  Yaks_fe_sock_codec.decode_header buf 
  >>= fun (header, buf) ->     
  decode_body header.mid  header.flags buf 
  >>= fun (body, buf) -> Apero.Result.ok ({header;  body}, buf)



let check_socket sock = 
  let _ = 
    match Lwt_unix.state sock with
    | Opened -> ignore @@ Logs_lwt.info (fun m -> m "Socket is open")
    | Closed -> ignore @@ Logs_lwt.info (fun m -> m "Socket is closed")
    | Aborted e -> ignore @@ Logs_lwt.info (fun m -> m "Socket is aborted: %s" (Printexc.to_string e))
  in ()

let send_to_socket msg sock = 
  let open Apero in
  let buf = IOBuf.create max_size in
  let concat bl bd = 
    let open Apero.Result.Infix in
    let data = IOBuf.create @@ (IOBuf.limit bl) + (IOBuf.limit bd) in
    IOBuf.put_buf bl data >>= IOBuf.put_buf bd 
  in
  match Yaks_fe_sock_codec.encode_message msg buf with
  | Ok buf -> 
    let lbuf = IOBuf.create 16 in
    let fbuf = IOBuf.flip buf in
    (match encode_vle (Vle.of_int @@ IOBuf.limit fbuf) lbuf with
     | Ok lbuf -> 
       let%lwt _ = Logs_lwt.debug (fun m -> m "Sending message to socket") in
       let _ = check_socket in
       let lbuf = IOBuf.flip lbuf in
       let data = IOBuf.flip @@ Apero.Result.get @@ concat lbuf fbuf in 
       Net.write_all sock data >>= fun bs ->
       let%lwt _ = Logs_lwt.debug (fun m -> m "Sended %d bytes" bs) in
       Lwt.return_unit
     | Error e -> 
       let%lwt _ = Logs_lwt.err (fun m -> m "Falied in writing message: %s" (Apero.show_error e)) in
       Lwt.fail @@ Exception e )     
  | Error e -> 
    let%lwt _ = Logs_lwt.err (fun m -> m "Falied in encoding messge: %s" (Apero.show_error e)) in
    Lwt.fail @@ Exception e 

let receiver_loop (driver:t) = 
  let open Apero in
  MVar.read driver >>= fun self ->
  let lbuf = IOBuf.create 16 in 
  let%lwt len = Net.read_vle self.sock lbuf in    
  let%lwt _ = Logs_lwt.debug (fun m -> m "Message lenght : %d" (Vle.to_int len)) in      
  let buf = IOBuf.create (Vle.to_int len) in
  let%lwt n = Net.read_all self.sock buf in
  let _ = check_socket self.sock
  in      
  let%lwt _ = Logs_lwt.debug (fun m -> m "Read %d bytes out of the socket" n) >>= fun _ -> Lwt.return_unit in      
  match decode_message buf with 
  | Ok (msg, _) -> 
    MVar.guarded driver 
    @@ (fun self ->
        let resolver = Option.get @@ WorkingMap.find_opt msg.header.corr_id self.working_set in
        let _ = Lwt.wakeup_later resolver msg in
        MVar.return () {self with working_set = WorkingMap.remove msg.header.corr_id self.working_set})

  | Error e -> 
    let%lwt _ = Logs_lwt.err (fun m -> m "Falied in parsing message %s" (Apero.show_error e)) in
    Lwt.fail @@ Exception e

let rec loop driver  () = 
  receiver_loop driver >>= loop driver

let create locator = 
  let sock = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  Apero_net.connect sock locator >>= fun s -> 
  let driver = MVar.create {sock = s; working_set = WorkingMap.empty; subscribers = ListenersMap.empty } in
  let _ = loop driver () in
  Lwt.return driver

let destroy driver = 
  MVar.read driver >>= fun d ->
  Apero_net.safe_close d.sock

let process (msg:Yaks_fe_sock_types.message) driver = 
  MVar.guarded driver @@
  fun self -> 
  let promise, completer = Lwt.wait () in
  send_to_socket msg self.sock 
  >>= fun _ ->
  MVar.return_lwt promise {self with working_set = WorkingMap.add msg.header.corr_id completer self.working_set}