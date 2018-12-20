open Apero
open Yaks_storage
open Yaks_types
open Yaks_core_types

module type Backend = sig
  val id : BeId.t
  val properties : properties
  val to_string : string

  val create_storage : Path.t -> properties -> Storage.t Lwt.t
end

module type BackendFactory  = sig 
  val kind : string
  val make : BeId.t -> properties  -> (module Backend)
end
