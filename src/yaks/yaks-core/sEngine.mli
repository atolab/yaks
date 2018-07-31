open Yaks_types
open Yaks_be

module SEngine : sig 
  type t 
    
  val make : unit -> t Lwt_mvar.t
  val create_access : t Lwt_mvar.t -> Path.t -> int64 ->  AccessId.t Lwt.t
  val create_access_with_id : t Lwt_mvar.t -> Path.t -> int64 ->  AccessId.t -> unit Lwt.t
  val get_access : t Lwt_mvar.t -> AccessId.t -> AccessId.t option Lwt.t

  val create_storage : t Lwt_mvar.t -> Path.t -> Property.t list -> StorageId.t Lwt.t 
  val create_storage_with_id : t Lwt_mvar.t -> Path.t -> Property.t list -> StorageId.t -> unit Lwt.t 
  val get_storage : t Lwt_mvar.t -> StorageId.t -> StorageId.t option Lwt.t

  val create_subscriber : t Lwt_mvar.t -> Path.t -> Selector.t -> bool -> SubscriberId.t Lwt.t  

  val get : t Lwt_mvar.t -> AccessId.t -> Selector.t -> (string * Value.t) list  Lwt.t
  val put : t Lwt_mvar.t -> AccessId.t -> string -> Value.t -> unit Lwt.t

  val add_backend_factory : t Lwt_mvar.t -> string -> (module BackendFactory) -> unit Lwt.t

end