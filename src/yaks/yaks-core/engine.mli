open Yaks_event
open Actor

module Engine : sig   
  type t
  type state
  type config = { channel_len : int }  

  (* val create : config -> Yaks_event.event Actor.actor_mailbox -> t *)
  val create : config -> t
  (** [create] a Yaks engine and return the function to be called to push events *)

  val mailbox : t -> Yaks_event.event Actor.actor_mailbox
  (** More operations will be needed *)  



  (* PRIVATE FUNCTIONS *)
  val add_plugin : Yaks_event.event Actor.actor_mailbox -> Int64.t -> Yaks_event.plugin_kind -> state -> (bool * state)
  val remove_plugin : Int64.t -> Yaks_event.plugin_kind -> state -> (bool * state)

  val start : t -> unit Lwt.t
end