open Apero
open Apero_net
open Lwt.Infix
open Yaks_fe_sock_codes
open Yaks_fe_sock_types
open Yaks_fe_sock_codec
open Yaks_fe_sock_processor

module Make (YEngine : Yaks_engine.SEngine.S) = struct 


  module Config = NetServiceTcp.TcpConfig

  module P = Processor.Make(YEngine) 

  module TcpSocketFE = NetServiceTcp.Make (MVar_lwt)  

  module MVar = MVar_lwt 

  type t = 
    { tcp_fe: TcpSocketFE.t
    ; io_svc: TcpSocketFE.io_service }


  let reader sock  =     
    let lbuf = IOBuf.create 16 in 
    let%lwt len = Net.read_vle sock lbuf in    
    let%lwt _ = Logs_lwt.debug (fun m -> m "Message lenght : %d" (Vle.to_int len)) in      
    let buf = IOBuf.create (Vle.to_int len) in     
    let%lwt n = Net.read sock buf in
    let _ = 
      match Lwt_unix.state sock with
      | Opened -> ignore @@ Logs_lwt.info (fun m -> m "Socket is open")
      | Closed -> ignore @@ Logs_lwt.info (fun m -> m "Socket is closed")
      | Aborted e -> ignore @@ Logs_lwt.info (fun m -> m "Socket is aborted: %s" (Printexc.to_string e))
    in      
    let%lwt _ = Logs_lwt.debug (fun m -> m "Read %d bytes our of the socket" n) >>= fun _ -> Lwt.return_unit in      

    match decode_message buf with 
    | Ok (msg, _) -> Lwt.return msg
    | Error e -> 
      let%lwt _ = Logs_lwt.err (fun m -> m "Falied in parsing message %s" (Apero.show_error e)) in
      Lwt.fail @@ Exception e

  let writer buf sock msg = 
    match encode_message msg buf with 
    | Ok buf ->  
      let lbuf = IOBuf.create 16 in 
      let fbuf = (IOBuf.flip buf) in       
      (match encode_vle (Vle.of_int @@ IOBuf.limit fbuf) lbuf with 
       | Ok lbuf -> 
         let%lwt _ = Logs_lwt.debug (fun m -> m "Sending message to socket") in
         let _ = 
           match Lwt_unix.state sock with
           | Opened -> ignore @@ Logs_lwt.info (fun m -> m "Socket is open")
           | Closed -> ignore @@ Logs_lwt.info (fun m -> m "Socket is closed")
           | Aborted e -> ignore @@ Logs_lwt.info (fun m -> m "Socket is aborted: %s" (Printexc.to_string e))
         in
         Net.send_vec sock [IOBuf.flip lbuf; fbuf]
       | Error e -> 
         let%lwt _ = Logs_lwt.err (fun m -> m "Falied in writing message: %s" (Apero.show_error e)) in
         Lwt.fail @@ Exception e )     
    | Error e -> 
      let%lwt _ = Logs_lwt.err (fun m -> m "Falied in encoding messge: %s" (Apero.show_error e)) in
      Lwt.fail @@ Exception e 


  let dispatch_message engine tx_sex  msg = 
    match msg.header.mid with 
    | OPEN -> P.process_open engine msg 
    | CREATE -> P.process_create engine msg
    | DELETE -> P.process_delete engine msg
    | PUT -> P.process_put engine msg
    | GET -> P.process_get engine msg 
    | SUB -> 
      let sock = TxSession.socket tx_sex in 
      let buf = IOBuf.create Yaks_fe_sock_types.max_msg_size in 
      let push_sub buf sid pvs = 
        let body = YNotification (Yaks_core.SubscriberId.to_string sid, pvs)  in                 
        let h = make_header NOTIFY [] Vle.zero Yaks_core.Property.Map.empty in         
        let msg = make_message h body in 
        writer buf sock msg >|= fun _ -> () 
      in  P.process_sub engine msg (push_sub buf)
    | UNSUB -> P.process_unsub engine msg
    | EVAL -> P.process_eval engine msg    
    | _ ->  P.process_error msg BAD_REQUEST


  let fe_service config  dispatcher tx_sex = 
    let buf = IOBuf.create (Config.buf_size config) in 
    let sock = TxSession.socket tx_sex in 
    let mwriter = writer buf sock in 
    fun () -> 
      reader sock >>= (dispatcher tx_sex)  >>= mwriter >>= fun _ -> Lwt.return_unit

  let create (conf : Config.t) (engine: YEngine.t) = 
    let tcp_fe = TcpSocketFE.make conf in 
    let dispatcher = dispatch_message engine in 
    let io_svc = fe_service conf dispatcher in (* TxSession.t -> unit -> unit Lwt.t*)
    {tcp_fe; io_svc}

  let start svc = 
    let _ = Logs_lwt.debug (fun m -> m "[FES] Sock-FE starting TCP/IP server") in
    TcpSocketFE.start svc.tcp_fe svc.io_svc

  let stop svc = TcpSocketFE.stop svc.tcp_fe

end


