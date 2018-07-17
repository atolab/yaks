module type Key = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val compare : t -> t-> int
  val prefix : t -> t -> bool
  val matches : t -> t -> bool
end


module type Value = sig
  type t
  val to_string : t -> string
  val of_string : string -> t
  val update : t -> t -> t
  val none : t
end

module type Store = sig
  type t
  type key
  type value

  val create: string -> int -> t
  val get : key -> t -> (key * value) list option
  val remove  : key -> t -> t
  val put : key -> value -> t ->  t 
  val dput : key -> value -> t -> t
  val update_value : key -> value -> t -> t
  val keys : t -> key list
  val dump : t -> unit
  val empty : t 

end
