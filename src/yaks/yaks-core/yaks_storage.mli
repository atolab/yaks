open Yaks_types
open Yaks_be

module Storage : sig 
  module Id : sig 
    type t
    val of_alias : string -> t
    val of_string : ?pos:int -> string -> t option
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val to_string : ?upper:bool -> t -> string
  end [@@deriving show]


  type t 

  val make : ?alias:string -> Path.t -> (module Backend) -> t 
  val id : t -> Id.t
  val alias : t -> string option
  val path : t -> Path.t
  val be_kind : t -> backend_kind
  val be : t -> (module Backend)


end  [@@deriving show]
