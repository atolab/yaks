open Apero
open Apero_net
open Lwt.Infix
open Yaks_core
open Yaks_fe_sock_codes
open Yaks_fe_sock_types
open Yaks_fe_sock_codec
open Yaks_fe_sock_processor

module Make (YEngine : Yaks_engine.Engine.S) = struct 


  module Config = NetServiceTcp.TcpConfig

  module P = Processor.Make(YEngine) 

  
  type t = 
    { tcp_fe: unit NetServiceTcp.t
    ; io_svc: unit NetServiceTcp.io_service }

  module WorkingMap = Map.Make(Apero.Vle)

  type state_t = {
    pending_eval_results : Value.t Lwt.u WorkingMap.t
  }

  type state = state_t Guard.t

  let reader sock  = 
    let open Apero.Infix in
    let%lwt len = Net.read_vle sock >>= Vle.to_int %> Lwt.return in    
    let%lwt _ = Logs_lwt.debug (fun m -> m "[FES] Incoming message lenght : %d" len) in
    let buf = Abuf.create len in     
    Net.read_all sock buf len >>= fun n -> (
    if n <> len then Logs_lwt.warn (fun m -> m "[FES] Failed to read %d bytes out of the socket" len) else Lwt.return_unit) >>= fun () ->
    Lwt.catch (fun () -> decode_message buf |> Lwt.return)
              (fun e -> Logs_lwt.err (fun m -> m "Failed in parsing message %s" (Printexc.to_string e)) >>= fun () ->
                        Logs_lwt.debug (fun m -> m "Buffer was:\n%s \nStack trace:\n%s" (Abuf.to_string buf) (Printexc.get_backtrace ())) >>= fun () ->
                        Lwt.fail e)


  let rec writer buf clientid sock msg =
    let%lwt _ = Logs_lwt.debug (fun m -> m "[FES] send %s #%Ld to %s" (message_id_to_string msg.header.mid) msg.header.corr_id (ClientId.to_string clientid)) in
    Abuf.clear buf; 
    (try encode_message_split msg buf |> Result.return
    with e -> Result.fail e)
    |> function 
    | Ok remain_msg ->
      let len = Abuf.readable_bytes buf in
      let lbuf = Abuf.create 16 in       
      Lwt.catch (fun () -> encode_vle (Vle.of_int len) lbuf; 
                           Logs_lwt.debug (fun m -> m "[FES] Sending message to socket: %d bytes" len) >>= fun () ->
                           Net.write_all sock (Abuf.wrap [lbuf; buf]) >>= fun _ -> 
                           match remain_msg with
                           | Some msg -> writer buf clientid sock msg
                           | None -> Lwt.return_unit)
                (fun e ->  Logs_lwt.err (fun m -> m "Failed in writing message: %s" (Printexc.to_string e)) >>= fun () -> Lwt.fail e)
    | Error e -> 
      let%lwt _ = Logs_lwt.err (fun m -> m "Failed in encoding %s message: %s" (message_id_to_string msg.header.mid) (Printexc.to_string e)) in
      let header = make_header ERROR [] msg.header.corr_id Properties.empty in
      let errormsg = make_message header  @@ YErrorInfo (Vle.of_int @@ error_code_to_int INTERNAL_SERVER_ERROR) in
      writer buf clientid sock errormsg


  let process_eval (s:state) clientid sock buf (path:Path.t) selector ~fallback =
    let _ = Logs_lwt.debug (fun m -> m "[FES] send EVAL(%s) for eval registered with %s" (Selector.to_string selector) (Path.to_string path)) in
    let (body:payload) = YSelector (selector) in
    let h = make_header EVAL [] (Random.int64 Int64.max_int) Properties.empty in
    let msg = make_message h body in
    Lwt.catch (
      fun () ->
        let (promise:Value.t Lwt.t), (resolver:Value.t Lwt.u) = Lwt.wait () in
        Guard.guarded s @@ fun self ->
        writer buf clientid sock msg >>= fun _ ->
        Guard.return_lwt promise { pending_eval_results = WorkingMap.add msg.header.corr_id resolver self.pending_eval_results }
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


  let make_ynotification sid path changes =
    let pcs = List.map (fun c -> (path, c)) changes in
    YNotification (Yaks_core.SubscriberId.to_string sid, pcs)

  let dispatch_message state engine clientid tx_sex msg =
    let%lwt _ = Logs_lwt.debug (fun m -> m "[FES] received %s #%Ld from %s" (message_id_to_string msg.header.mid) msg.header.corr_id (ClientId.to_string clientid)) in
    match msg.header.mid with 
    | LOGIN -> P.process_login engine clientid msg >|= fun response ->
      if response.header.mid = OK then Lwt.async (fun () -> Lwt.bind (TxSession.when_closed tx_sex) (fun _ -> P.process_logout engine clientid msg));
      response
    | LOGOUT -> P.process_logout engine clientid msg
    | WORKSPACE -> P.process_workspace engine clientid msg
    | PUT -> P.process_put engine clientid msg
    | GET -> P.process_get engine clientid msg 
    | DELETE -> P.process_delete engine clientid msg
    | SUB -> 
      let sock = TxSession.socket tx_sex in 
      let buf = Abuf.create Yaks_fe_sock_types.max_msg_size in 
      let push_sub buf sid ~fallback path changes =
        let _ = Logs_lwt.debug (fun m -> m "[FES] notify subscriber %s/%s" (ClientId.to_string clientid) (Yaks_core.SubscriberId.to_string sid)) in
        let body = make_ynotification sid path changes in                 
        let h = make_header NOTIFY [] (Random.int64 Int64.max_int) Properties.empty in
        let msg = make_message h body in
        Lwt.catch 
          (fun () -> writer buf clientid sock msg >|= fun _ -> ())
          (fun ex -> let _ = Logs_lwt.debug (fun m -> m "[FES] Error notifying subscriber %s/%s : %s" (ClientId.to_string clientid) (Yaks_core.SubscriberId.to_string sid) (Printexc.to_string ex)) in
            fallback sid)
      in  P.process_sub engine clientid msg (push_sub buf)
    | UNSUB -> P.process_unsub engine clientid msg
    | REG_EVAL ->
      let sock = TxSession.socket tx_sex in
      let buf = Abuf.create Yaks_fe_sock_types.max_msg_size in 
      P.process_reg_eval engine clientid msg (process_eval state clientid sock buf)
    | UNREG_EVAL ->
      P.process_unreg_eval engine clientid msg
    | EVAL -> P.process_eval engine clientid msg 
    | VALUES ->
      Guard.guarded state @@ (fun self ->
      (match WorkingMap.find_opt msg.header.corr_id self.pending_eval_results with 
      | Some resolver ->
        Guard.return_lwt (P.process_values msg resolver) { pending_eval_results = WorkingMap.remove msg.header.corr_id self.pending_eval_results }
      | None ->
        Guard.return_lwt (P.process_error msg BAD_REQUEST) self)
      )
    | ERROR ->
      Guard.guarded state @@ (fun self ->
      (match WorkingMap.find_opt msg.header.corr_id self.pending_eval_results with 
      | Some resolver ->
        Guard.return_lwt (P.process_error_on_eval msg resolver) { pending_eval_results = WorkingMap.remove msg.header.corr_id self.pending_eval_results }
      | None ->
        Guard.return_lwt (P.process_error msg BAD_REQUEST) self)
      )
    | _ -> 
      let _ = Logs_lwt.warn (fun m -> m "[FES] received unexpected message with mid %d" (message_id_to_int msg.header.mid)) in
      P.process_error msg BAD_REQUEST


  let fe_service feid config dispatcher tx_sex =
    let buf = Abuf.create ~grow:4096 (Config.buf_size config) in
    let sock = TxSession.socket tx_sex in 
    let sid = TxSession.id tx_sex |> NetService.Id.to_string |> SessionId.of_string in
    let (clientid: ClientId.t) = { feid; sid } in
    let mwriter = writer buf clientid sock in
    fun () ->
      (* Note: block on read, but return unit as soon as a message is read. 
         The message is dispatched as another promise.
         This is to allow the engine to not block on a GET request that might require to be resolved
         by an incoming VALUES message from an API (in case an Eval matches the GET).
      *) 
      reader sock >>= fun msg -> 
        let _ = (dispatcher clientid tx_sex) msg >>= fun reply -> mwriter reply in
      Lwt.return_unit

  let create feid (conf : Config.t) (engine: YEngine.t) =
    let _ = Logs_lwt.debug (fun m -> m "[FES] SOCK-FE starting TCP socket on %s" (TcpLocator.to_string @@ Config.locator conf)) in
    let tcp_fe = NetServiceTcp.make conf in
    let state = Guard.create { pending_eval_results=WorkingMap.empty } in
    let dispatcher = dispatch_message state engine in 
    let io_svc = fe_service feid conf dispatcher in (* TxSession.t -> unit -> unit Lwt.t*)
    {tcp_fe; io_svc}

  let start svc = 
    let _ = Logs_lwt.debug (fun m -> m "[FES] Sock-FE starting TCP/IP server") in
    NetServiceTcp.start svc.tcp_fe (fun _ -> Lwt.return_unit) svc.io_svc

  let stop svc = NetServiceTcp.stop svc.tcp_fe

end


