open Event_stream
open Yaks_codec.Message

module Engine = struct 
  
  module type S = sig
    type t        
    type 'a sink
    type config = { channel_len : int }  

    val create : config -> t
    (** [create] a Yaks engine and return the function to be called to push events *)

    val event_sink : t ->  (message  * message sink) sink
    (** More operations will be needed *)  

    val start : t -> unit Lwt.t
  end


  module Make (S : EventStream.S )  = struct       
    
    type 'a sink = 'a S.Sink.s
    type 'a source = 'a S.Source.s    
    type t =  { evt_src : (message * (message sink)) source; evt_sink : (message * (message sink)) sink }

    type config = { channel_len : int }  

    let create cfg = 
      let (evt_src, evt_sink)  = S.create cfg.channel_len in { evt_src; evt_sink }

    let event_sink e = e.evt_sink
    
    let process e msg snk = 
      let mid = msg.mid in 
      match msg.msg with 
      | Yaks_codec.(`Request req) -> 
        let%lwt _ = S.Sink.push msg snk in
        Lwt.return_unit
      | Yaks_codec.(`Reply reply) -> Lwt.return_unit
      

    let start e  =
      let open Lwt.Infix in 
      let rec loop () = 
        match%lwt S.Source.get e.evt_src with
        | Some (msg, snk) ->           
          let%lwt _ = Logs_lwt.debug (fun m -> m "Processing message id = %d " msg.mid) in 
          process e msg snk >>= loop
        | None -> loop ()
      in loop ()

  end
end

