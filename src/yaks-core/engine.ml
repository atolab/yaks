open Yaks_event

module Engine = struct 
  
  type t =  { evt_src : event_source; evt_sink : event_sink }
  
  type config = { channel_len : int }  
  
  let create cfg = 
      let (evt_src, evt_sink)  = EventStream.create cfg.channel_len in { evt_src; evt_sink }

    let event_sink e = e.evt_sink
    
    let process e (msg : message) (handler : message_handler) = 
      let mid = msg.mid in 
      match msg.msg with 
      | Yaks_codec.(`Request req) -> handler msg         
      | Yaks_codec.(`Reply reply) -> Lwt.return_unit
      

    let start e  =
      let open Lwt.Infix in 
      let rec loop () = 
        match%lwt EventStream.Source.get e.evt_src with
        | Some (EventWithHandler (msg, handler))  ->           
          let%lwt _ = Logs_lwt.debug (fun m -> m "Processing message id = %d " msg.mid) in 
          process e msg handler >>= loop
        
        | Some (Event msg)  ->           
          let%lwt _ = Logs_lwt.debug (fun m -> m "Processing message id = %d " msg.mid) in 
          process e msg (fun e -> Lwt.return_unit) >>= loop

        | None -> loop ()
      in loop ()

end

