open Event_stream
open Yaks_codec.Message


module Engine = struct
  module type S = sig
    type t
    type 'a sink       
    type config = { channel_len : int }  

    val create : config -> t
    (** [create] a Yaks engine and return the function to be called to push events *)

    val process : t -> Yaks_codec.Message.message  -> message sink -> unit Lwt.t  
    (** More operations will be needed *)  
  end

  module Make (S : EventStream.S )  = struct   
    type t =  { evt_src : message S.Source.s; evt_sink : message S.Sink.s }

    type 'a sink = 'a S.Sink.s
      
    type config = { channel_len : int }  

    let create cfg = 
      let (evt_src, evt_sink) = S.create cfg.channel_len in { evt_src; evt_sink }

    let process e msg s  =       
      let%lwt _ = Logs_lwt.debug (fun m -> m "Processing message id = %d " msg.mid) in 
      Lwt.return_unit  
  end
end

