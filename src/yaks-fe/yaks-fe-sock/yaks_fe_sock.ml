open Apero
open Apero_net
open Lwt.Infix

module Make (YEngine : Yaks_engine.SEngine.S) = struct 
  module SocketFE = TcpService.Make (MVar_lwt)

  type config = Apero_net.TcpService.Config.t

  type t = SocketFE.t * SocketFE.io_service

  let reader sock  = 
    let lbuf = IOBuf.create 16 in 
    let open Yaks_fe_sock_codec in 
    let%lwt len = Net.read_vle sock lbuf in         
    let buf = IOBuf.create (Vle.to_int len) in 
    let%lwt _ = Net.read sock buf in 
    match decode_message (IOBuf.flip buf) with 
    | Ok (msg, _) -> Lwt.return msg
    | Error e -> Lwt.fail @@ Exception e

  let writer buf sock msg = 
    let open Yaks_fe_sock_codec in 
    match encode_message msg buf with 
    | Ok buf -> Net.write sock (IOBuf.flip buf)
    | Error e -> Lwt.fail @@ Exception e 

  let dispatch_message engine msg = 
    let _ = engine in 
    Lwt.return msg

  let fe_service config  dispatcher sock = 
    let buf = IOBuf.create (TcpService.Config.buf_size config) in 
    let mwriter = writer buf sock in 
    fun () -> 
      reader sock >>= dispatcher >>= mwriter >>= fun _ -> Lwt.return_unit

  let create (conf : config) (engine: YEngine.t) = 
    let svc = SocketFE.create conf in 
    let dispatcher = dispatch_message engine in 
    let io_svc = fe_service conf dispatcher in 
    (svc, io_svc)

  let start (svc, iosvc) = SocketFE.start svc iosvc

  let stop (svc, _) = SocketFE.stop svc

end


