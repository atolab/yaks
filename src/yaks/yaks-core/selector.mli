open Ypath

module Selector : sig

  type t          

  val of_string : string -> t

  val to_string : t -> string

  val path : t -> Ypath.t

  val query : t -> string option

  val fragment : t -> string option

  val is_matching : t -> Ypath.t -> bool

end