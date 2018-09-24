open Yaks_types
open Yaks_access
open Yaks_storage
open Yaks_property
open Yaks_be


module SEngine : sig 

  module type S = sig 
    type t 
    type subscription_pusher = Yaks_types.SubscriberId.t -> (Yaks_types.Path.t * Yaks_types.Value.t) list -> unit Lwt.t

    val make : unit -> t 
 
    val add_backend : t -> (module Backend) -> unit Lwt.t

    val create_access : t  -> ?alias:string -> Path.t -> int64 ->  Access.t Lwt.t
    val get_access : t -> Access.Id.t -> Access.t option Lwt.t
    val dispose_access : t -> Access.Id.t -> unit Lwt.t

    val create_storage : t -> ?alias:string -> Path.t -> properties -> Storage.t Lwt.t 
    val get_storage : t -> Storage.Id.t -> Storage.t option Lwt.t
    val dispose_storage : t -> Storage.Id.t -> unit Lwt.t

    val get : t -> Access.Id.t -> Selector.t -> (Path.t * Value.t) list  Lwt.t
    val put : t -> Access.Id.t -> Selector.t -> Value.t -> unit Lwt.t
    val put_delta : t -> Access.Id.t -> Selector.t -> Value.t -> unit Lwt.t
    val remove : t -> Access.Id.t -> Selector.t -> unit Lwt.t

    val create_subscriber : t -> Access.Id.t -> Selector.t -> bool -> subscription_pusher -> SubscriberId.t Lwt.t  
  end

  module Make (MVar: Apero.MVar) : S 

end
