open Yaks_types

type backend_kind =  Memory | Disk 

module type Backend = sig 
  val kind : backend_kind
  val path : Path.t  
  val get : Selector.t -> (string * Value.t) list Lwt.t
  val put : Selector.t -> Value.t -> unit Lwt.t   
end

module type BackendFactory  = sig 
  val kind : backend_kind
  val make : Path.t -> Property.t list  -> (module Backend)
  val name : string
end