open Yaks_property
open Yaks_storage
open Yaks_types

module type Backend = sig
  val properties : properties
  val to_string : string

  val create_storage : ?alias:string -> Path.t -> properties -> Storage.t Lwt.t
end

module type BackendFactory  = sig 
  val make : Property.t list  -> (module Backend)
  val name : string
end
