open Apero
open Apero_net
open Lwt.Infix
open Yaks_core
open Yaks_fe_sock_codes
open Yaks_fe_sock_types
open Yaks_fe_sock_codec
open Yaks_fe_sock_processor

module Make (YEngine : Yaks_engine.Engine.S) (MVar: Apero.MVar) = struct 


  module Config = NetServiceTcp.TcpConfig

  module P = Processor.Make(YEngine) 

  module TcpSocketFE = NetServiceTcp.Make (MVar_lwt)  

  type t = 
    { tcp_fe: TcpSocketFE.t
    ; io_svc: TcpSocketFE.io_service }

  module WorkingMap = Map.Make(Apero.Vle)

  type state_t = {
    pending_eval_results : Value.t Lwt.u WorkingMap.t
  }

  type state = state_t MVar.t

  let reader sock  =     
    let lbuf = IOBuf.create 16 in 
    let%lwt len = Net.read_vle sock lbuf in    

    let%lwt _ = Logs_lwt.debug (fun m -> m "Message lenght : %d" (Vle.to_int len)) in      
    let buf = IOBuf.create (Vle.to_int len) in     
    let%lwt n = Net.read sock buf in
    let%lwt _ = Logs_lwt.debug (fun m -> m "Read %d bytes our of the socket" n) >>= fun _ -> Lwt.return_unit in      

    match decode_message buf with 
    | Ok (msg, _) -> Lwt.return msg
    | Error e -> 
      let%lwt _ = Logs_lwt.err (fun m -> m "Failed in parsing message %s" (Apero.show_error e)) in
      Lwt.fail @@ Exception e


  let rec writer buf sock msg =
    match encode_message_split msg buf with
    | Ok (buf, remain_msg) ->
      let lbuf = IOBuf.create 16 in 
      let fbuf = (IOBuf.flip buf) in       
      (match encode_vle (Vle.of_int @@ IOBuf.limit fbuf) lbuf with 
      | Ok lbuf -> 
        let%lwt _ = Logs_lwt.debug (fun m -> m "Sending message to socket: %d bytes" (IOBuf.limit fbuf)) in
        Net.send_vec sock [IOBuf.flip lbuf; fbuf]
      | Error e -> 
        let%lwt _ = Logs_lwt.err (fun m -> m "Failed in writing message: %s" (Apero.show_error e)) in
        Lwt.fail @@ Exception e )
      >>= fun r -> (match remain_msg with
        | Some msg -> 
          writer (IOBuf.clear buf) sock msg
        | None -> Lwt.return r)
    | Error e -> 
      let%lwt _ = Logs_lwt.err (fun m -> m "Failed in encoding %s message: %s" (message_id_to_string msg.header.mid) (Apero.show_error e)) in
      let header = make_header ERROR [] msg.header.corr_id Properties.empty in
      let errormsg = make_message header  @@ YErrorInfo (Vle.of_int @@ error_code_to_int INTERNAL_SERVER_ERROR) in
      writer (IOBuf.clear buf) sock errormsg


  let process_eval (s:state) sock buf (path:Path.t) selector ~fallback =
    let _ = Logs_lwt.debug (fun m -> m "[FES] send EVAL(%s) for eval registered with %s" (Selector.to_string selector) (Path.to_string path)) in
    let (body:payload) = YSelector (selector) in
    let h = make_header EVAL [] (Random.int64 Int64.max_int) Properties.empty in
    let msg = make_message h body in
    Lwt.catch (
      fun () ->
        let (promise:Value.t Lwt.t), (resolver:Value.t Lwt.u) = Lwt.wait () in
        MVar.guarded s @@ fun self ->
        writer buf sock msg >>= fun _ ->
        MVar.return_lwt promise { pending_eval_results = WorkingMap.add msg.header.corr_id resolver self.pending_eval_results }
      )
    (fun ex ->
      match ex with
      | YException err ->
        (* Error message received from eval implementer. Forward it, don't call fallback *)
        let _ = Logs_lwt.debug (fun m -> m "[FES] received ERROR calling EVAL(%s) for eval registered with %s: %s"
          (Selector.to_string selector) (Path.to_string path) (show_yerror err))
        in
        Lwt.fail ex
      | _ ->
        (* Exception received trying to send message to eval implementer, probably gone... call fallback that will remove it *)
        let _ = Logs_lwt.debug (fun m -> m "[FES] received unexpected exception calling EVAL(%s) for eval registered with %s: %s"
          (Selector.to_string selector) (Path.to_string path) (Printexc.to_string ex))
        in
        fallback path)


  let dispatch_message feid state engine tx_sex  msg = 
    let sid = TxSession.id tx_sex |> NetService.Id.to_string |> SessionId.of_string in 
    let (clientid: ClientId.t) = { feid; sid }  in
    match msg.header.mid with 
    | LOGIN -> P.process_login engine clientid msg 
    | LOGOUT -> P.process_logout engine clientid msg
    | WORKSPACE -> P.process_workspace engine clientid msg
    | PUT -> P.process_put engine clientid msg
    | GET -> P.process_get engine clientid msg 
    | DELETE -> P.process_delete engine clientid msg
    | SUB -> 
      let sock = TxSession.socket tx_sex in 
      let buf = IOBuf.create Yaks_fe_sock_types.max_msg_size in 
      let push_sub buf sid ~fallback pvs = 
        let body = YNotification (Yaks_core.SubscriberId.to_string sid, pvs)  in                 
        let h = make_header NOTIFY [] (Random.int64 Int64.max_int) Properties.empty in
        let msg = make_message h body in         
        Lwt.catch (fun () -> writer buf sock msg >|= fun _ -> ()) (fun _ -> fallback sid)
      in  P.process_sub engine clientid msg (push_sub buf)
    | UNSUB -> P.process_unsub engine clientid msg
    | REG_EVAL ->
      let sock = TxSession.socket tx_sex in
      let buf = IOBuf.create Yaks_fe_sock_types.max_msg_size in 
      P.process_reg_eval engine clientid msg (process_eval state sock buf)
    | UNREG_EVAL ->
      P.process_unreg_eval engine clientid msg
    | EVAL -> P.process_eval engine clientid msg 
    | VALUES ->
      MVar.guarded state @@ (fun self ->
      (match WorkingMap.find_opt msg.header.corr_id self.pending_eval_results with 
      | Some resolver ->
        MVar.return_lwt (P.process_values msg resolver) { pending_eval_results = WorkingMap.remove msg.header.corr_id self.pending_eval_results }
      | None ->
        MVar.return_lwt (P.process_error msg BAD_REQUEST) self)
      )
    | ERROR ->
      MVar.guarded state @@ (fun self ->
      (match WorkingMap.find_opt msg.header.corr_id self.pending_eval_results with 
      | Some resolver ->
        MVar.return_lwt (P.process_error_on_eval msg resolver) { pending_eval_results = WorkingMap.remove msg.header.corr_id self.pending_eval_results }
      | None ->
        MVar.return_lwt (P.process_error msg BAD_REQUEST) self)
      )
    | _ -> 
      let _ = Logs_lwt.warn (fun m -> m "[FES] received unexpected message with mid %d" (message_id_to_int msg.header.mid)) in
      P.process_error msg BAD_REQUEST


  let fe_service config  dispatcher tx_sex = 
    let buf = IOBuf.create (Config.buf_size config) in 
    let sock = TxSession.socket tx_sex in 
    let mwriter = writer buf sock in 
    fun () ->
      (* Note: block on read, but return unit as soon as a message is read. 
         The message is dispatched as another promise.
         This is to allow the engine to not block on a GET request that might require to be resolved
         by an incoming VALUES message from an API (in case an Eval matches the GET).
      *) 
      reader sock >>= fun msg -> 
        let _ = (dispatcher tx_sex) msg >>= fun reply -> mwriter reply in
      Lwt.return_unit

  let create id (conf : Config.t) (engine: YEngine.t) = 
    let _ = Logs_lwt.debug (fun m -> m "[FES] SOCK-FE starting TCP socket on %s" (TcpLocator.to_string @@ Config.locator conf)) in
    let tcp_fe = TcpSocketFE.make conf in
    let state = MVar.create { pending_eval_results=WorkingMap.empty } in
    let dispatcher = dispatch_message id state engine in 
    let io_svc = fe_service conf dispatcher in (* TxSession.t -> unit -> unit Lwt.t*)
    {tcp_fe; io_svc}

  let start svc = 
    let _ = Logs_lwt.debug (fun m -> m "[FES] Sock-FE starting TCP/IP server") in
    TcpSocketFE.start svc.tcp_fe svc.io_svc

  let stop svc = TcpSocketFE.stop svc.tcp_fe

end


