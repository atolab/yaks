open Lwt.Infix
open Yaks_sock_message
open Yaks_sock_types

module MVar = Apero.MVar_lwt

module Access = struct 

  type listener = (Yaks_types.Path.t * Yaks_types.Value.t) list -> unit Lwt.t
  type eval_callback = Yaks_types.Path.t -> Yaks_types.Value.t

  module EvalId = Apero.Uuid
  module SubscriberMap =  Map.Make(SubscriberId)
  module EvalMap = Map.Make(EvalId)

  type state = {
    subscriptions : (listener) SubscriberMap.t
  ; encoding : Yaks_fe_sock_codes.value_encoding
  ; path : Yaks_types.Path.t
  ; cache_size : int
  ; evals : (eval_callback) EvalMap.t
  ; aid : AccessId.t
  ; driver : Yaks_sock_driver.t
  }

  type t = state MVar.t

  let get_id access = 
    MVar.read access >>= fun a -> Lwt.return a.aid

  let create cache_size encoding path id driver = 
    let acc = {
      aid = id
    ; path = path
    ; cache_size = cache_size
    ; encoding = encoding
    ; subscriptions = SubscriberMap.empty
    ; evals = EvalMap.empty
    ; driver
    } in 
    MVar.create acc

  let get selector access = 
    MVar.read access >>= fun access ->
    let _ = ignore @@ Logs_lwt.info (fun m -> m "[YAS]: GET on %s" (Yaks_types.Selector.to_string selector)) in
    Message.make_get (IdAccess access.aid) selector
    >>= fun msg -> Yaks_sock_driver.sendmsg msg access.driver
    >>= fun _ -> Yaks_sock_driver.recvmsg access.driver
    >>= fun rmsg ->
    if rmsg.header.corr_id <> msg.header.corr_id then
      Lwt.fail_with "CorrId is different!"
    else
      match rmsg.body with
      | YPathValueList _ -> Lwt.fail_with "Body is YPathValueList"
      | YEmpty -> Lwt.fail_with "Body is YEmpty"
      | YPath _ -> Lwt.fail_with "Body is YPath"
      | YSelector _ -> Lwt.fail_with "Body is YSelector"
      | YSelectorValueList l -> Lwt.return l
      | YSubscription _ -> Lwt.fail_with "Body is YSubscription"
      | YNotification _ -> Lwt.fail_with "Body is YNotification"
      | YErrorInfo e -> 
        let errno = Apero.Vle.to_int e in 
        Lwt.fail_with @@ Printf.sprintf "[YAS]: GET ErrNo: %d" errno

  let put selector value access =
    MVar.read access >>= fun access ->
    let _ = ignore @@ Logs_lwt.info (fun m -> m "[YAS]: PUT on %s -> %s" (Yaks_types.Selector.to_string selector) (Yaks_types.Value.to_string value)) in
    Message.make_put (IdAccess access.aid) selector value 
    >>= fun msg -> Yaks_sock_driver.sendmsg msg access.driver
    >>= fun _ -> Yaks_sock_driver.recvmsg access.driver
    >>= fun rmsg ->
    if rmsg.header.corr_id <> msg.header.corr_id then
      Lwt.fail_with "CorrId is different!"
    else
      Lwt.return_unit

  let delta_put selector value access = 
    MVar.read access >>= fun _ ->
    let _ = ignore @@ Logs_lwt.info (fun m -> m "[YAS]: DELAT_PUT on %s -> %s" (Yaks_types.Selector.to_string selector) (Yaks_types.Value.to_string value)) in
    Lwt.return_unit

  let remove selector access =
    MVar.read access >>= fun _ ->
    let _ = ignore @@ Logs_lwt.info (fun m -> m "[YAS]: REMOVE on %s" (Yaks_types.Selector.to_string selector)) in
    Lwt.return_unit

  let subscribe ?(listener=(fun _ -> Lwt.return_unit)) selector access =
    MVar.guarded access @@ fun access ->
    let _ = ignore @@ Logs_lwt.info (fun m -> m "[YAS]: SUBSCRIBE on %s" (Yaks_types.Selector.to_string selector)) in
    let subid = SubscriberId.next_id () in 
    let new_access = {access with subscriptions = SubscriberMap.add subid listener access.subscriptions} in  
    MVar.return subid new_access

  let unsubscribe subscriber_id access= 
    MVar.guarded access @@ fun access ->
    let _ = ignore @@ Logs_lwt.info (fun m -> m "[YAS]: UNSUBSCRIBE ID: %s" (SubscriberId.to_string subscriber_id)) in
    let new_access = {access with subscriptions = SubscriberMap.remove subscriber_id access.subscriptions} in  
    MVar.return () new_access

  let get_subscriptions access = 
    MVar.read access >>= fun access ->
    let _ = ignore @@ Logs_lwt.info (fun m -> m "[YAS]: Getting all subscriptions" ) in 
    Lwt.return (List.map (fun (id, _) -> id) (SubscriberMap.bindings access.subscriptions))

  let eval selector f access =
    MVar.guarded access @@ fun access ->
    let _ = ignore @@ Logs_lwt.info (fun m -> m "[YAS]: EVAL on: %s" (Yaks_types.Selector.to_string selector)) in
    let new_access = {access with evals = EvalMap.add (EvalId.make ()) f access.evals} in  
    MVar.return () new_access
end