open Yaks_codec.Message
open Event_stream

module Engine : sig 
  
  module type S = sig
    type t
    type 'a sink
    type config = { channel_len : int }  

    val create : config -> t
    (** [create] a Yaks engine and return the function to be called to push events *)

    val process : t -> Yaks_codec.Message.message  -> message sink -> unit Lwt.t  
    (** More operations will be needed *)  
  end

  module Make (S : EventStream.S ) : S with type 'a sink = 'a S.Sink.s
end