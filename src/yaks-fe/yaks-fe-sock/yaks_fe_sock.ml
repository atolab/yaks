open Apero
open Apero_net
open Lwt.Infix

module Make (YEngine : Yaks_engine.SEngine.S) = struct 
  module SocketFE = TcpService.Make (MVar_lwt)

  module Config = Apero_net.TcpService.Config
  
  type t = SocketFE.t * SocketFE.io_service
  
  let reader sock  = 
    let lbuf = IOBuf.create 16 in 
    let open Yaks_fe_sock_codec in 
    let%lwt len = Net.read_vle sock lbuf in          
    let buf = IOBuf.create (Vle.to_int len) in 
    let%lwt _ = Net.read sock buf in     
    match decode_message buf with 
    | Ok (msg, _) -> Lwt.return msg
    | Error e -> Lwt.fail @@ Exception e

  let writer buf sock msg = 
    let open Yaks_fe_sock_codec in 
    match encode_message msg buf with 
    | Ok buf ->  
      let lbuf = IOBuf.create 16 in 
      let fbuf = (IOBuf.flip buf) in       
      (match encode_vle (Vle.of_int @@ IOBuf.limit fbuf) lbuf with 
      | Ok lbuf -> 
        Net.send_vec sock [IOBuf.flip lbuf; fbuf]
      | Error e -> Lwt.fail @@ Exception e )     
    | Error e -> Lwt.fail @@ Exception e 

  let reply_with_ok msg = 
    let open Yaks_fe_sock_types in 
    let header = make_header OK [] msg.header.corr_id in 
     make_message header [] Empty 
  
  let reply_with_error msg code = 
    let open Yaks_fe_sock_codes in 
    let open Yaks_fe_sock_types in 
    let header = make_header ERROR [] msg.header.corr_id in 
     make_message header [] @@ ErrorInfo (Vle.of_int @@ error_code_to_int code)

  let process_open (* engine *) _ msg = Lwt.return @@ reply_with_ok msg
  
  let process_create_access engine  msg path = 
    let open Yaks_core in     
    let open Yaks_fe_sock_types in 
    let open Yaks_fe_sock_codes in 
    match Path.of_string path with 
    | Some path -> 
      (* need to get the size out of the props *)
      let cache_size = 1024L in 
      let%lwt access = YEngine.create_access engine ?alias:None path cache_size in 
      let aid = Access.Id.to_string (Access.id access) in
      let header = make_header OK [PROPERTY]  msg.header.corr_id  in 
      let reply = make_message header [Property.make Property.Access.Key.id aid] Empty in
      Lwt.return @@ reply
    | None -> Lwt.return @@ reply_with_error msg BAD_REQUEST
     

  let process_create_storage _ (* engine *) msg  _ (* path *) = 
    let open Yaks_fe_sock_codes in     
    Lwt.return @@ reply_with_error msg BAD_REQUEST 
  
  let process_create engine msg = 
    let open Yaks_fe_sock_types in 
    match get_path_payload msg with 
    | Some path -> 
      if has_access_flag msg.header then 
        process_create_access engine msg path
      else if has_storage_flag msg.header then 
        process_create_storage engine msg path
      else 
        Lwt.return @@ reply_with_error msg BAD_REQUEST 
    | None -> Lwt.return @@  reply_with_error msg BAD_REQUEST

    (* let _ = engine in reply_with_ok msg *)
  let process_delete engine msg = let _ = engine in Lwt.return @@ reply_with_ok msg
  let process_put engine msg = let _ = engine in Lwt.return @@ reply_with_ok msg
  let process_get engine msg = let _ = engine in Lwt.return @@  reply_with_ok msg
  let process_sub engine msg = let _ = engine in Lwt.return @@ reply_with_ok msg
  let process_unsub engine msg = let _ = engine in Lwt.return @@ reply_with_ok msg
  let process_eval engine msg = let _ = engine in Lwt.return @@ reply_with_ok msg

  let dispatch_message engine msg = 
    let open Yaks_fe_sock_types in 
    match msg.header.mid with 
    | OPEN -> process_open engine msg 
    | CREATE -> process_create engine msg
    | DELETE -> process_delete engine msg
    | PUT -> process_put engine msg
    | GET -> process_get engine msg 
    | SUB -> process_sub engine msg 
    | UNSUB -> process_unsub engine msg
    | EVAL -> process_eval engine msg    
    | _ -> Lwt.return @@ reply_with_error msg BAD_REQUEST

    

  let fe_service config  dispatcher sock = 
    let buf = IOBuf.create (Config.buf_size config) in 
    let mwriter = writer buf sock in 
    fun () -> 
      reader sock >>= dispatcher >>= mwriter >>= fun _ -> Lwt.return_unit

  let create (conf : Config.t) (engine: YEngine.t) = 
    let svc = SocketFE.create conf in 
    let dispatcher = dispatch_message engine in 
    let io_svc = fe_service conf dispatcher in 
    (svc, io_svc)

  let start (svc, iosvc) = 

    let _ = Logs_lwt.debug (fun m -> m "[FES] Sock-FE starting TCP/IP server") in
    SocketFE.start svc iosvc

  let stop (svc, _) = SocketFE.stop svc

end


