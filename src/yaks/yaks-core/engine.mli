(* open Yaks_event *)
open Actor

module Engine : sig
  type t
  type state
  type config = { channel_len : int }
  type access

  (* type handler = Yaks_event.message -> 'a Lwt.t *)

  val create : config -> t
  (** [create] a Yaks engine and return the function to be called to push events *)

  val mailbox : t -> Yaks_event.event Actor.actor_mailbox
  (** More operations will be needed *)  

    val start : t -> unit Lwt.t


  (* PRIVATE FUNCTIONS *)
  
  val generate_id : unit -> string
  
  val add_plugin : Yaks_event.event Actor.actor_mailbox -> Int64.t -> Yaks_event.plugin_kind -> state -> (bool * state)
  val remove_plugin : Int64.t -> Yaks_event.plugin_kind -> state -> (bool * state)
  
  (* val create_access : ?id:String.t -> string -> Int64.t -> state -> (String.t * state)
  val get_access : String.t -> state -> access
  val dispose_access : String.t -> state -> state *)
  
  val add_subscriber : ?id:Int64.t -> string ->Yaks_event.event Apero.Actor.actor_mailbox -> state -> (Int64.t * state)
  val remove_subscriber : Int64.t -> state -> (Int64.t * state)
  val get_subscribers : string -> state -> Yaks_event.event Apero.Actor.actor_mailbox list

  val push_to_be : Yaks_event.message -> state -> Yaks_event.message Lwt.t
  (* val forward_to_be  *)


  val push_to_tx : Yaks_event.message -> Yaks_event.event Apero.Actor.actor_mailbox option-> state -> Yaks_event.message Lwt.t



end