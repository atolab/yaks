open Yaks_sock_types

type t

val create : Apero_net.Locator.t -> t Lwt.t
val destroy : t ->  unit Lwt.t

val process_get : Yaks_types.Selector.t -> AccessId.t -> t -> ((Yaks_types.Path.t * Yaks_types.Value.t) list) Lwt.t 
val process_put : Yaks_types.Selector.t -> AccessId.t -> Yaks_types.Value.t -> t -> unit Lwt.t 
val process_patch : Yaks_types.Selector.t -> AccessId.t ->Yaks_types.Value.t -> t -> unit Lwt.t 
val process_remove : ?delete_type:entity_type -> ?selector:Yaks_core.Selector.t -> id -> t  -> unit Lwt.t 
val process_subscribe : ?listener:listener_t -> Yaks_types.Selector.t -> AccessId.t -> t -> SubscriberId.t Lwt.t
val process_unsubscribe :  SubscriberId.t -> AccessId.t -> t -> unit Lwt.t
val process_eval : Yaks_types.Selector.t -> eval_callback_t -> AccessId.t -> t -> unit Lwt.t

val process : Yaks_fe_sock_types.message -> t -> Yaks_fe_sock_types.message Lwt.t

