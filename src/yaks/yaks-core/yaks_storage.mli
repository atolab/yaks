open Yaks_types
open Yaks_property

module Storage : sig 

  module Id : (module type of Apero.Uuid)

  type t 

  val make : ?alias:string -> Path.t -> properties ->
    (unit -> unit Lwt.t) ->
    (Selector.t -> (Path.t * Value.t) list Lwt.t) ->
    (Selector.t -> Value.t -> unit Lwt.t) ->
    (Selector.t -> Value.t -> unit Lwt.t) ->
    (Selector.t -> unit Lwt.t) -> t

  val dispose : t -> unit Lwt.t

  val id : t -> Id.t
  val alias : t -> string option
  val path : t -> Path.t
  val properties : t -> properties

  val to_string : t -> string

  val is_covering : t -> Selector.t -> bool
  (** [is_covering s sel] tests if [s] covers the Selector [sel] (i.e. if [sel] might match some path stored by [s]) *)

  val is_conflicting : t -> Path.t -> bool
  (** [is_conflicting s path] tests if [s]' path and [path] have an intersection *)

  val get : t -> Selector.t -> (Path.t * Value.t) list Lwt.t

  val put : t -> Selector.t -> Value.t -> unit Lwt.t   
  val put_delta : t -> Selector.t -> Value.t -> unit Lwt.t   

  val remove : t -> Selector.t -> unit Lwt.t


end  [@@deriving show]
