open Apero
open Yaks_core
open Yaks_fe_sock_codes
open Yaks_fe_sock_types

module Processor = struct 
  module type S = sig 

    module YEngine : Yaks_engine.Engine.S 

    val process_login : YEngine.t -> ClientId.t -> message -> message Lwt.t
    val process_logout : YEngine.t -> ClientId.t -> message -> message Lwt.t
    val process_workspace : YEngine.t -> ClientId.t -> message -> message Lwt.t
    val process_put : YEngine.t -> ClientId.t -> message -> message Lwt.t
    val process_get : YEngine.t -> ClientId.t -> message -> message Lwt.t
    val process_delete : YEngine.t -> ClientId.t -> message -> message Lwt.t
    val process_sub : YEngine.t -> ClientId.t -> message -> notify_subscriber -> message Lwt.t    
    val process_unsub : YEngine.t -> ClientId.t -> message -> message Lwt.t
    val process_reg_eval : YEngine.t -> ClientId.t -> message -> eval_function -> message Lwt.t
    val process_unreg_eval : YEngine.t -> ClientId.t -> message -> message Lwt.t
    val process_eval : YEngine.t -> ClientId.t -> message -> message Lwt.t
    val process_values : message -> Value.t Lwt.u -> message Lwt.t
    val process_error_on_eval :  message -> Value.t Lwt.u -> message Lwt.t
    val process_error :  message -> error_code -> message Lwt.t
  end

  module Make (Engine : Yaks_engine.Engine.S ) = struct 

    module YEngine = Engine  

    let reply_with_ok msg ps =     
      let header = make_header OK [] msg.header.corr_id ps in 
      make_message header YEmpty

    let reply_with_values msg vs =     
      let header = make_header VALUES [] msg.header.corr_id Properties.empty in 
      make_message header (YPathValueList vs)
    
    let reply_with_error msg code =   
      let header = make_header ERROR [] msg.header.corr_id Properties.empty in 
      make_message header  @@ YErrorInfo (Vle.of_int @@ error_code_to_int code)

    let ex_to_reply msg ex =
      match ex with
    | YException e -> 
      Logs.warn (fun m -> m "FES: caught YException: %s " (show_yerror e));
      reply_with_error msg BAD_REQUEST
    | ex ->
      Logs.warn (fun m -> m "FES: caught unexpected exception: %s \n %s" (Printexc.to_string ex) (Printexc.get_backtrace ()));
      reply_with_error msg BAD_REQUEST

    let workspace_from_props p =
      let open Apero.Option.Infix in
      Properties.find_opt Yaks_properties.Admin.workspaceid p >>=
      WsId.of_string_opt

    let process_login engine clientid msg = 
      Lwt.try_bind
        (fun () -> YEngine.login engine clientid msg.header.properties)
        (fun () -> Lwt.return @@ reply_with_ok msg Properties.empty)
        (fun ex -> Lwt.return @@ ex_to_reply msg ex)

    let process_logout engine clientid msg = 
      Lwt.try_bind
        (fun () -> YEngine.logout engine clientid)
        (fun () -> Lwt.return @@ reply_with_ok msg Properties.empty)
        (fun ex -> Lwt.return @@ ex_to_reply msg ex)

    let process_workspace engine clientid msg = 
      match get_path_payload msg with 
      | Some path ->
        Lwt.try_bind
          (fun () -> YEngine.add_workspace engine clientid path)
          (fun wsid -> Lwt.return @@ reply_with_ok msg @@
            Properties.singleton Yaks_properties.Admin.workspaceid @@ WsId.to_string wsid)
          (fun ex -> Lwt.return @@ ex_to_reply msg ex)
      | None -> Lwt.return @@ reply_with_error msg BAD_REQUEST

    let process_put engine clientid msg = 
      match get_path_value_list_payload msg with
      | Some pvs ->
        let workspace = workspace_from_props msg.header.properties in
        Lwt.try_bind
          (fun () -> Lwt.join @@ List.map (fun (p,v) -> YEngine.put engine clientid ?workspace p v) pvs)
          (fun () -> Lwt.return @@ reply_with_ok msg Properties.empty)
          (fun ex -> Lwt.return @@ ex_to_reply msg ex)
      | None -> Lwt.return @@ reply_with_error msg BAD_REQUEST


    let process_get engine clientid msg =
      match get_selector_payload msg with
      | Some s ->
        let workspace = workspace_from_props msg.header.properties in
        Lwt.try_bind
          (fun () -> YEngine.get engine clientid ?workspace s)
          (fun pvs -> Lwt.return @@ reply_with_values msg pvs)
          (fun ex -> Lwt.return @@ ex_to_reply msg ex)
      | None -> Lwt.return @@ reply_with_error msg BAD_REQUEST

    let process_delete engine clientid msg = 
      match get_path_payload msg with 
      | Some path ->
        let workspace = workspace_from_props msg.header.properties in
        Lwt.try_bind
          (fun () -> YEngine.remove engine clientid ?workspace path)
          (fun () -> Lwt.return @@ reply_with_ok msg Properties.empty)
          (fun ex -> Lwt.return @@ ex_to_reply msg ex)
      | None -> Lwt.return @@ reply_with_error msg BAD_REQUEST

    let process_sub engine clientid msg pusher  = 
      match get_selector_payload msg with
      | Some s ->
        let workspace = workspace_from_props msg.header.properties in
        let is_pusher = true in                                              (* TODO: manage is_pusher *)
        Lwt.try_bind
          (fun () -> YEngine.subscribe engine clientid ?workspace s is_pusher pusher)
          (fun subid -> Lwt.return @@ reply_with_ok msg @@
            Properties.singleton Yaks_properties.Admin.subscriberid @@ WsId.to_string subid)
          (fun ex -> Lwt.return @@ ex_to_reply msg ex)
      | None -> Lwt.return @@ reply_with_error msg BAD_REQUEST

    let process_unsub engine clientid msg = 
      let open Apero.Option.Infix in 
      match get_subscription_payload msg >>= SubscriberId.of_string_opt with
      | Some sid -> 
        Lwt.try_bind
          (fun () -> YEngine.unsubscribe engine clientid sid)
          (fun () -> Lwt.return @@ reply_with_ok msg Properties.empty)
          (fun ex -> Lwt.return @@ ex_to_reply msg ex)
      | None -> Lwt.return @@ reply_with_ok msg Properties.empty 


    let process_reg_eval engine clientid msg eval =
      match get_path_payload msg with
      | Some p ->
        let workspace = workspace_from_props msg.header.properties in
        Lwt.try_bind
          (fun () -> YEngine.register_eval engine clientid ?workspace p eval)
          (fun () -> Lwt.return @@ reply_with_ok msg Properties.empty)
          (fun ex -> Lwt.return @@ ex_to_reply msg ex)
      | None -> Lwt.return @@ reply_with_error msg BAD_REQUEST

    let process_unreg_eval engine clientid msg =
      match get_path_payload msg with
      | Some p ->
        let workspace = workspace_from_props msg.header.properties in
        Lwt.try_bind
          (fun () -> YEngine.unregister_eval engine clientid ?workspace p)
          (fun () -> Lwt.return @@ reply_with_ok msg Properties.empty)
          (fun ex -> Lwt.return @@ ex_to_reply msg ex)
      | None -> Lwt.return @@ reply_with_error msg BAD_REQUEST

    let process_eval engine clientid msg =
      match get_selector_payload msg with
      | Some s ->
        let workspace = workspace_from_props msg.header.properties in
        Lwt.try_bind
          (fun () -> YEngine.eval engine clientid ?workspace s)
          (fun pvs -> Lwt.return @@ reply_with_values msg pvs)
          (fun ex -> Lwt.return @@ ex_to_reply msg ex)
      | None -> Lwt.return @@ reply_with_error msg BAD_REQUEST

    let process_values msg resolver =
      match get_path_value_list_payload msg with
      | Some ((_,v)::l) ->
        if List.length l > 0 then Logs.warn (fun m -> m "FES: processing VALUES - received more than 1 Value, ignoring the extras") else ();
        let _ = Lwt.wakeup_later resolver v in
        Lwt.return @@ reply_with_ok msg Properties.empty
      | Some [] ->
        Logs.debug (fun m -> m "FES: processing VALUES - received an empty list");
        Lwt.return @@ reply_with_error msg BAD_REQUEST
      | None ->
        Logs.debug (fun m -> m "FES: processing VALUES - failed to decode body");
        Lwt.return @@ reply_with_error msg BAD_REQUEST

    let process_error_on_eval msg resolver =
      match get_error_info msg with
      | Some errcode ->
        let _ = Lwt.wakeup_later_exn resolver @@ YException (`InternalError (`Msg ("Received ERROR with code "^(error_code_to_string errcode)^"on eval "))) in
        Lwt.return @@ reply_with_ok msg Properties.empty
      | None ->
        Logs.debug (fun m -> m "FES: processing ERROR - failed to decode body");
        Lwt.return @@ reply_with_error msg BAD_REQUEST


    let process_error msg code = Lwt.return @@ reply_with_error msg code 
  end
end 

