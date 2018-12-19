open Apero
open Yaks_core_types

module type Frontend = sig
  val id : FeId.t
  val properties : properties
  val to_string : string

  val start : unit -> unit Lwt.t
  val stop : unit -> unit Lwt.t
end

module type FrontendFactory  = sig 
  val make : FeId.t -> Property.t list  -> (module Frontend)
  val name : string
end
