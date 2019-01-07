open Apero
open Apero_net
open Websocket
open Websocket_lwt
open Lwt.Infix 
open Yaks_core
open Yaks_fe_sock_processor
open Yaks_fe_sock_codec 
open Yaks_fe_sock_codes
open Yaks_fe_sock_types

module Make (YEngine : Yaks_engine.Engine.S) (MVar: Apero.MVar) = struct 

  module Config = Apero_net.NetServiceWebSock.Config
  module P = Processor.Make(YEngine)

  type t = {
    feid: FeId.t
  ; svc: Apero_net.NetServiceWebSock.t
  ; engine : YEngine.t
  ; config : Config.t }

  module WorkingMap = Map.Make(Apero.Vle)

  type state_t = {
    pending_eval_results : Value.t Lwt.u WorkingMap.t
  }

  type state = state_t MVar.t

  let sessionid_of_source (s:Connected_client.source) =
    match s with
    | Connected_client.TCP (ip,port) -> SessionId.of_string @@
      Printf.sprintf "TCP:%s:%s" (Ipaddr.to_string ip) (string_of_int port)
    | Connected_client.Domain_socket dom -> SessionId.of_string @@
      "DOM:"^dom
    | Connected_client.Vchan flow -> SessionId.of_string @@
      Printf.sprintf "VCH:%s:%s" (string_of_int flow.domid) flow.port

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

  let process_eval (s:state) client buf (path:Path.t) selector ~fallback =
    let _ = Logs_lwt.debug (fun m -> m "[FEWS] send GET(%s) for eval %s" (Selector.to_string selector) (Path.to_string path)) in
    let (body:payload) = YSelector (selector) in
    let h = make_header GET [] Vle.zero Properties.empty in
    let msg = make_message h body in
    Lwt.catch (
      fun () ->
        let (promise:Value.t Lwt.t), (resolver:Value.t Lwt.u) = Lwt.wait () in
        MVar.guarded s @@ fun self ->
        send_msg buf client Frame.Opcode.Binary msg >>= fun _ ->
        MVar.return_lwt promise { pending_eval_results = WorkingMap.add msg.header.corr_id resolver self.pending_eval_results }
      )
    (fun _ -> fallback path)


  let dispatch_message feid wbuf state engine client msg = 
    let sid = sessionid_of_source @@ Connected_client.source client in
    let (clientid: ClientId.t) = { feid; sid }  in
    match msg.header.mid with 
    | LOGIN -> P.process_login engine clientid msg 
    | LOGOUT -> P.process_logout engine clientid msg
    | WORKSPACE -> P.process_workspace engine clientid msg
    | PUT -> P.process_put engine clientid msg
    | GET -> P.process_get engine clientid msg 
    | DELETE -> P.process_delete engine clientid msg
    | SUB ->       
      let push_sub buf sid ~fallback pvs = 
        let body = YNotification (Yaks_core.SubscriberId.to_string sid, pvs)  in                 
        let h = make_header NOTIFY [] Vle.zero Properties.empty in         
        let msg = make_message h body in         
        Lwt.catch (fun () -> send_msg buf client Frame.Opcode.Binary msg   >|= fun _ -> ()) (fun _ -> fallback sid)
      in  P.process_sub engine clientid msg (push_sub wbuf)
    | UNSUB -> P.process_unsub engine clientid msg
    | REG_EVAL -> P.process_reg_eval engine clientid msg (process_eval state client wbuf)
    | UNREG_EVAL -> P.process_unreg_eval engine clientid msg
    | VALUES ->
      MVar.guarded state @@ (fun self ->
      (match WorkingMap.find_opt msg.header.corr_id self.pending_eval_results with 
      | Some resolver ->
        MVar.return_lwt (P.process_values msg resolver) { pending_eval_results = WorkingMap.remove msg.header.corr_id self.pending_eval_results }
      | None ->
        MVar.return_lwt (P.process_error msg BAD_REQUEST) self)
      )
    | _ ->  P.process_error msg BAD_REQUEST

  let handler feid state engine _ wbuf  client =
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
          | Ok(msg, _) -> 
           (* Note: return unit as soon as a message is read. 
              The message is dispatched as another promise.
              This is to allow the engine to not block on a GET request that might require to be resolved
              by an incoming VALUES message from an API (in case an Eval matches the GET).
            *) 
            let _ = dispatch_message feid wbuf state engine client msg >>= send_msg wbuf client fr.opcode in
            Lwt.return_unit
          | Error _ -> close 1000)        
        | _ -> close 1002

  let create feid config engine =  
    let svc = NetServiceWebSock.make config in 
    {feid; svc; engine; config}

  let start fe  = 
    let _ = Logs_lwt.debug (fun m -> m "[FEWS] WebSock-FE starting server at %s" (WebSockLocator.to_string @@ Config.locator fe.config)) in    
    let state = MVar.create { pending_eval_results=WorkingMap.empty } in
    NetServiceWebSock.start fe.svc (handler fe.feid state fe.engine) 
  
  let stop fe = NetServiceWebSock.stop fe.svc
end
