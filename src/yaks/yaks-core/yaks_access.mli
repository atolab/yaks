open Yaks_types

module Access : sig 
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


(* 
  This module rapresent an YAKS Access
  An access is created by a user and is below a group and it inherit the access rights from the user group
  GB: Not sure if we add to link the access to the user that created it, to avoid nasty user steal access
 *)

  type t 

  type access_right =  R_Mode | W_Mode | RW_Mode

  val make : Path.t -> int64 -> t 
  val id : t -> Id.t 
  val path : t -> Path.t
  val cache_size : t -> int64
  val right : t -> access_right

  val check_access_right : t -> Selector.t -> access_right -> unit Lwt.t
  (* [check_access_right] checks if the current access can access the data 
     identified by the selector. If the rights are not sufficient a Lwt.fail is 
     returned with the proper error *)

end  [@@deriving show]
