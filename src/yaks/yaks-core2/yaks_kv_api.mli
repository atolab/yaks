open Apero
open Yaks_types
open Yaks_be


module KVApi : sig
    type t

    type subscription_pusher =  SubscriberId.t -> fallback:(SubscriberId.t -> unit Lwt.t) -> (Path.t * Value.t) list -> unit Lwt.t
    type eval_getter = Path.t -> Selector.t -> fallback:(Path.t -> Value.t Lwt.t) -> Value.t Lwt.t


    val get : t -> Access.Id.t -> Selector.t -> (Path.t * Value.t) list  Lwt.t
    val put : t -> Access.Id.t -> Path.t -> Value.t -> unit Lwt.t
    val put_delta : t -> Access.Id.t -> Path.t -> Value.t -> unit Lwt.t
    val remove : t -> Access.Id.t -> Path.t -> unit Lwt.t

    val create_subscriber : t -> Access.Id.t -> Selector.t -> bool -> subscription_pusher -> SubscriberId.t Lwt.t  
    val remove_subscriber : t -> Access.Id.t -> SubscriberId.t -> unit Lwt.t  

    val eval : t -> Access.Id.t -> Path.t -> eval_getter -> unit Lwt.t

end

