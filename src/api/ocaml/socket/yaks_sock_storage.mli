open Yaks_sock_types

module Storage : sig 

  type t

  val get_id : t -> StorageId.t Lwt.t
  val create: String.t Yaks_core.Property.Map.t-> Yaks_types.Path.t -> StorageId.t -> Yaks_sock_driver.t -> t
end