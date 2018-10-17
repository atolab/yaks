open Yaks_sock_types


module Access : sig 
  type t

  val create : int -> Yaks_fe_sock_codes.value_encoding -> Yaks_types.Path.t -> AccessId.t -> Yaks_sock_driver.t -> t
  val get : Yaks_types.Selector.t -> t -> ((Yaks_types.Path.t * Yaks_types.Value.t) list) Lwt.t 
  val put : Yaks_types.Selector.t -> Yaks_types.Value.t -> t -> unit Lwt.t 
  val delta_put: Yaks_types.Selector.t -> Yaks_types.Value.t -> t -> unit Lwt.t
  val remove: Yaks_types.Selector.t -> t  -> unit Lwt.t 
  val subscribe: ?listener:listener_t -> Yaks_types.Selector.t -> t -> SubscriberId.t Lwt.t
  val unsubscribe: SubscriberId.t -> t -> unit Lwt.t
  val get_subscriptions : t -> (SubscriberId.t list) Lwt.t
  val eval : Yaks_types.Selector.t -> eval_callback_t -> t -> unit Lwt.t
  val get_id : t -> AccessId.t Lwt.t
end