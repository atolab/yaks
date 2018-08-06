open Yaks_group

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

  type t = 
    { id : Id.t 
    ; name : string
    ; password : string
    ; groups : Group.Id.t list
    } 


  val make : string -> string -> Group.Id.t list -> t 
  (* val id : t -> Id.t 
  val name : t -> string
  val pwd : t -> string
  val groups : t -> Group.Id.t list *)
  val add_group : t -> Group.Id.t -> t
  val remove_group : t -> Group.Id.t -> t


end  [@@deriving show]
