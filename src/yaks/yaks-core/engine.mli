open Yaks_event
open Actor

module Engine : sig   
  type t          
  type config = { channel_len : int }  

  val create : config -> t
  (** [create] a Yaks engine and return the function to be called to push events *)

  val mailbox : t -> Yaks_event.event Actor.actor_mailbox
  (** More operations will be needed *)  

  val start : t -> unit Lwt.t
end