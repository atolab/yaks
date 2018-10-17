open Lwt.Infix
open Yaks_sock_types

module MVar = Apero.MVar_lwt

module Access = struct 



  module EvalId = Apero.Uuid
  module SubscriberMap =  Map.Make(SubscriberId)
  module EvalMap = Map.Make(EvalId)

  type state = {
    subscriptions : (listener_t) SubscriberMap.t
  ; encoding : Yaks_fe_sock_codes.value_encoding
  ; path : Yaks_types.Path.t
  ; cache_size : int
  ; evals : (eval_callback_t) EvalMap.t
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
    let _ = Logs_lwt.info (fun m -> m "[YA]: GET on %s" (Yaks_types.Selector.to_string selector)) in
    Yaks_sock_driver.process_get selector access.aid access.driver

  let put selector value access =
    MVar.read access >>= fun access ->
    let _ = ignore @@ Logs_lwt.info (fun m -> m "[YA]: PUT on %s -> %s" (Yaks_types.Selector.to_string selector) (Yaks_types.Value.to_string value)) in
    Yaks_sock_driver.process_put selector access.aid value access.driver

  let delta_put selector value access = 
    MVar.read access >>= fun access ->
    let _ = Logs_lwt.info (fun m -> m "[YA]: DELAT_PUT on %s -> %s" (Yaks_types.Selector.to_string selector) (Yaks_types.Value.to_string value)) in
    Yaks_sock_driver.process_patch selector access.aid value access.driver

  let remove selector access =
    MVar.read access >>= fun access ->
    let _ = Logs_lwt.info (fun m -> m "[YA]: REMOVE on %s" (Yaks_types.Selector.to_string selector)) in
    Yaks_sock_driver.process_remove ~delete_type:`Resource  ~selector (IdAccess access.aid)  access.driver


  let subscribe ?(listener=(fun _ -> Lwt.return_unit)) selector access =
    MVar.read access >>= fun access ->
    let _ = Logs_lwt.info (fun m -> m "[YA]: SUBSCRIBE on %s" (Yaks_types.Selector.to_string selector)) in
    Yaks_sock_driver.process_subscribe ~listener selector access.aid access.driver


  let unsubscribe subscriber_id access= 
    MVar.read access >>= fun access ->
    let _ = Logs_lwt.info (fun m -> m "[YA]: UNSUBSCRIBE ID: %s" (SubscriberId.to_string subscriber_id)) in
    Yaks_sock_driver.process_unsubscribe subscriber_id access.aid access.driver

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