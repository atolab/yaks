open Yaks_types
open Yaks_access
open Yaks_property
open Yaks_be
open Yaks_user
open Yaks_group

module SEngine : sig 

  module type S = sig 
    type t 
      
    val make : unit -> t 
    val create_access : t  -> Path.t -> int64 -> User.Id.t ->  Access.Id.t Lwt.t
    val create_access_with_id : t -> Path.t -> int64 -> User.Id.t -> Access.Id.t -> unit Lwt.t
    val get_access : t -> Access.Id.t -> Access.Id.t option Lwt.t
    val dispose_access : t -> Access.Id.t -> unit Lwt.t

    val create_storage : t -> Path.t -> Property.t list -> StorageId.t Lwt.t 
    val create_storage_with_id : t -> Path.t -> Property.t list -> StorageId.t -> unit Lwt.t 
    val get_storage : t -> StorageId.t -> StorageId.t option Lwt.t
    val dispose_storage : t -> StorageId.t -> unit Lwt.t

    val create_group : t -> string -> Selector.t list -> Selector.t list -> Selector.t list -> Group.group_level -> Group.Id.t Lwt.t
    val create_group_with_id : t -> string -> Selector.t list -> Selector.t list -> Selector.t list -> Group.group_level -> Group.Id.t -> unit Lwt.t

    val create_user : t -> string -> string -> Group.Id.t -> User.Id.t Lwt.t
    val create_user_with_id : t -> string -> string -> Group.Id.t -> User.Id.t -> unit Lwt.t
    val authenticate_user : t -> string -> string -> User.Id.t Lwt.t 

    val create_subscriber : t -> Path.t -> Selector.t -> bool -> SubscriberId.t Lwt.t  

    val get : t -> Access.Id.t -> Selector.t -> (string * Value.t) list  Lwt.t
    
    val put : t -> Access.Id.t -> Selector.t -> Value.t -> unit Lwt.t
    val put_delta : t -> Access.Id.t -> Selector.t -> Value.t -> unit Lwt.t

    val remove : t -> Access.Id.t -> Selector.t -> unit Lwt.t
    
    val add_backend_factory : t -> string -> (module BackendFactory) -> unit Lwt.t
  end

  module Make (MVar: Apero.MVar) : S 

end