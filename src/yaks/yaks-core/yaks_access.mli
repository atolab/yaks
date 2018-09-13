open Yaks_types

module Access : sig 
  module Id : (module type of Apero.Uuid)

  type t 

  val make : ?alias:string -> Path.t -> int64 ->  t 
  val id : t -> Id.t
  val alias : t -> string option
  val path : t -> Path.t
  val cache_size : t -> int64

  val to_string : t -> string

  val is_covering : t -> Selector.t -> bool
  (** [is_covering a sel] tests if [a] covers the Selector [sel] (i.e. if [sel] might match some path accessible by [a]) *)

end  [@@deriving show]
