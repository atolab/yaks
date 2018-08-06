open Yaks_types

module Group : sig 
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


  type group_level =  Admins | Users

  type t = 
    { id : Id.t 
    ; name : string
    ; rw_paths : Selector.t list
    ; r_paths : Selector.t list
    ; w_paths : Selector.t list
    ; group_level : group_level
    } 


  val make : string -> Selector.t list -> Selector.t list -> Selector.t list -> group_level ->  t 
  (* val id : t -> Id.t 
  val name : t -> string
  val rw_paths : t -> Selector.t list
  val r_paths : t -> Selector.t list 
  val w_paths : t -> Selector.t list *)


end  [@@deriving show]
