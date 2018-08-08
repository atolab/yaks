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

  (* 
    This module rapresent a YAKS User Group
    Groups are repsonsible for the access rights.
    Each group as:
    - ID
    - Name
    - Level (if is and admin group (admins can create storages, users, groups..) or a user group)
    - RW_Paths - selectors for which this group as RW rights
    - R_Paths - selectors for which this group as read only rights
    - W_Paths - selectors for which this group as write only rights

   *)

  (* Not sure if we may need an Operator level that can create storage for instance *)
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
