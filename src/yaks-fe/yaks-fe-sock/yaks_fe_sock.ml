open Apero
open Apero_net
open Lwt.Infix
open Yaks_fe_sock_codes
open Yaks_fe_sock_types
open Yaks_fe_sock_codec
open Yaks_fe_sock_processor

module Make (YEngine : Yaks_engine.SEngine.S) = struct 
    
  module P = Processor.Make(YEngine) 

  module SocketFE = TcpService.Make (MVar_lwt)

  module Config = Apero_net.TcpService.Config

  type t = SocketFE.t * SocketFE.io_service

  let reader sock  = 
    let lbuf = IOBuf.create 16 in 
    let%lwt len = Net.read_vle sock lbuf in          
    let buf = IOBuf.create (Vle.to_int len) in 
    let%lwt _ = Net.read sock buf in     
    match decode_message buf with 
    | Ok (msg, _) -> Lwt.return msg
    | Error e -> Lwt.fail @@ Exception e

  let writer buf sock msg = 
    match encode_message msg buf with 
    | Ok buf ->  
      let lbuf = IOBuf.create 16 in 
      let fbuf = (IOBuf.flip buf) in       
      (match encode_vle (Vle.of_int @@ IOBuf.limit fbuf) lbuf with 
       | Ok lbuf -> 
         Net.send_vec sock [IOBuf.flip lbuf; fbuf]
       | Error e -> Lwt.fail @@ Exception e )     
    | Error e -> Lwt.fail @@ Exception e 

  
  let dispatch_message engine msg = 
    match msg.header.mid with 
    | OPEN -> P.process_open engine msg 
    | CREATE -> P.process_create engine msg
    | DELETE -> P.process_delete engine msg
    | PUT -> P.process_put engine msg
    | GET -> P.process_get engine msg 
    | SUB -> P.process_sub engine msg 
    | UNSUB -> P.process_unsub engine msg
    | EVAL -> P.process_eval engine msg    
    | _ ->  P.process_error msg BAD_REQUEST


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


