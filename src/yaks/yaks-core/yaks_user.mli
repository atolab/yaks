
module User : sig 
  module Id : sig 
    type t
    val make : unit -> t
    val next_id : unit -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val of_bytes : ?pos:int -> string -> t option
    val to_bytes : t -> string
    val of_string : ?pos:int -> string -> t option
    val to_string : ?upper:bool -> t -> string
  end [@@deriving show]

  type t 


  val make : string -> string -> t 
  val id : t -> Id.t 
  val name : t -> string
  val pwd : t -> string


end  [@@deriving show]
