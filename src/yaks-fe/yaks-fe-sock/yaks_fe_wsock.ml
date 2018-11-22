open Apero
open Apero_net
open Websocket
open Websocket_lwt
open Lwt.Infix 
open Yaks_fe_sock_processor
open Yaks_fe_sock_codec 
open Yaks_fe_sock_codes
open Yaks_fe_sock_types

module Make (YEngine : Yaks_engine.SEngine.S) = struct 

  module Config = Apero_net.NetServiceWebSock.Config
  module P = Processor.Make(YEngine)

  type t = {
    svc: Apero_net.NetServiceWebSock.t
  ; engine : YEngine.t
  ; config : Config.t }
    
  let send_frame client opcode content = 
    Connected_client.send client
      Frame.(create ~opcode:opcode ~content:content ())

  let send_as_frame wbuf client opcode = 
    let frame = Frame.(create ~opcode:opcode ~content:(Lwt_bytes.to_string (IOBuf.to_bytes wbuf)) ()) in
    Connected_client.send client frame
      
  let send_msg wbuf client opcode msg  =     
    match encode_message msg (IOBuf.clear wbuf) with 
    | Ok buf -> 
      send_as_frame buf client opcode
    | Error e -> 
      let%lwt _ = Logs_lwt.err (fun m -> m "Failed in parsing message %s" (Apero.show_error e)) in
      Lwt.fail @@ Exception e

let dispatch_message wbuf engine client msg = 
    match msg.header.mid with 
    | OPEN -> P.process_open engine msg 
    | CREATE -> P.process_create engine msg
    | DELETE -> P.process_delete engine msg
    | PUT -> P.process_put engine msg
    | GET -> P.process_get engine msg 
    | SUB ->       
      let push_sub buf sid ~cleanup pvs = 
        let body = YNotification (Yaks_core.SubscriberId.to_string sid, pvs)  in                 
        let h = make_header NOTIFY [] Vle.zero Properties.empty in         
        let msg = make_message h body in         
        Lwt.catch (fun () -> send_msg buf client Frame.Opcode.Binary msg   >|= fun _ -> ()) (fun _ -> cleanup sid)
      in  P.process_sub engine msg (push_sub wbuf)
    | UNSUB -> P.process_unsub engine msg
    | EVAL -> P.process_eval engine msg    
    | _ ->  P.process_error msg BAD_REQUEST


  let handler engine _ wbuf  client =     
    Connected_client.recv client 
    >>= fun fr ->
      let close code =
        if String.length fr.content >= 2 then
          send_frame client Frame.Opcode.Close (String.sub fr.content 0 2)          
        else        
          Connected_client.send client @@ Frame.close code
      in       
      Lwt_log.debug_f ~section "Client : %s" Frame.(show fr) 
      >>= fun () ->
        match fr.opcode with
        | Frame.Opcode.Ping -> send_frame client Frame.Opcode.Pong fr.content          
        
        | Frame.Opcode.Close ->
          (* Immediately echo and pass this last message to the user *)
          let%lwt _ = Logs_lwt.debug (fun m -> m "[FEWS]: Connection closed by remote peer") in 
          close 1000

        | Frame.Opcode.Pong -> Lwt.return_unit
        
        | Frame.Opcode.Text -> 
          let%lwt _ = Logs_lwt.warn (fun m -> m "[FEWS]: Received text message while expected binary. Closing session.") in  
          close 1000

        | Frame.Opcode.Binary ->      
          let buf = IOBuf.from_bytes @@ Lwt_bytes.of_string fr.content in 
          (match decode_message buf with 
          | Ok(msg, _) -> dispatch_message wbuf engine client msg >>= send_msg wbuf client fr.opcode
          | Error _ -> close 1000)        
        | _ -> close 1002

  let create config engine =  
    let svc = NetServiceWebSock.make config in 
    {svc; engine; config}

  let start fe  = 
    let _ = Logs_lwt.debug (fun m -> m "[FEWS] WebSock-FE starting server at %s" (WebSockLocator.to_string @@ Config.locator fe.config)) in    
    NetServiceWebSock.start fe.svc (handler fe.engine) 
  
  let stop fe = NetServiceWebSock.stop fe.svc
end
