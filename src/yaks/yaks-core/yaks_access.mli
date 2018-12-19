open Yaks_types

module Access : sig 
  module Id : (module type of Apero.Uuid)

  type access_kind = Internal | FrontEnd | Transport
  type t 

  val make : ?kind:access_kind -> Path.t -> Apero.properties -> t 
  val id : t -> Id.t
  val alias : t -> string option
  val path : t -> Path.t
  val cache_size : t -> int64

  val to_string : t -> string

  val is_covering_path : t -> Path.t -> bool
  (** [is_covering_path a p] tests if [a] covers the Path [p] (i.e. if [p] is accessible by [a]) *)
  val is_covering_selector : t -> Selector.t -> bool
  (** [is_covering_selector a s] tests if [a] covers the Selector [s] (i.e. if [s] might match some path accessible by [a]) *)
  
  val register_subscriber : t -> Yaks_types.SubscriberId.t -> t 
  val unregister_subscriber : t -> Yaks_types.SubscriberId.t -> t 
  val subscribers : t -> Yaks_types.SubscriberId.t list 

end  [@@deriving show]
