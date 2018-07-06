open Yaks_event

module Engine : sig   
  type t          
  type config = { channel_len : int }  

  val create : config -> t
  (** [create] a Yaks engine and return the function to be called to push events *)

  val event_sink : t -> event_sink
  (** More operations will be needed *)  

    val start : t -> unit Lwt.t
end