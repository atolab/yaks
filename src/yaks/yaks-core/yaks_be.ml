open Yaks_types
open Yaks_property

type backend_kind = Memory | Disk 

module type Backend = sig 
  val kind : backend_kind

  val get : Selector.t -> (string * Value.t) list Lwt.t

  val put : Selector.t -> Value.t -> unit Lwt.t  
  val put_delta : Selector.t -> Value.t -> unit Lwt.t    

  val remove : Selector.t -> unit Lwt.t
end

module type BackendFactory  = sig 
  val kind : backend_kind
  val make : Property.t list  -> (module Backend)
  val name : string
end