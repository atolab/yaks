module Ypath : sig

  type t

  val of_string : string -> t

  val to_string : t -> string

  val is_prefix : t -> t -> bool

  val matches : t -> t -> bool

end