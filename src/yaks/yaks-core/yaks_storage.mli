open Yaks_types
open Yaks_be

module Storage : sig 
  module Id : (module type of Apero.Uuid)

  type t 

  val make : ?alias:string -> Path.t -> (module Backend) -> t 
  val id : t -> Id.t
  val alias : t -> string option
  val path : t -> Path.t
  val be_kind : t -> backend_kind
  val be : t -> (module Backend)


end  [@@deriving show]
