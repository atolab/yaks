open Yaks_types

module Access : sig 
  module Id : sig 
    type t
    val of_alias : string -> t
    val of_string : ?pos:int -> string -> t option
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val to_string : ?upper:bool -> t -> string
  end [@@deriving show]

  type t 

  val make : ?alias:string -> Path.t -> int64 ->  t 
  val id : t -> Id.t
  val alias : t -> string option
  val path : t -> Path.t
  val cache_size : t -> int64

  
end  [@@deriving show]
