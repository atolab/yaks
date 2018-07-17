open Yaks_event

module Engine = struct 

  type t =  { evt_src : event_source; evt_sink : event_sink }

  type config = { channel_len : int }  

  let create cfg = 
    let (evt_src, evt_sink)  = EventStream.create cfg.channel_len in { evt_src; evt_sink }

  let event_sink e = e.evt_sink

  let process e (msg : message) (handler : message_handler) = 

    match msg with 
    | Create {cid; entity; entity_id} -> handler (Ok {cid; entity_id})
    | Dispose { cid; entity_id }  -> handler (Ok {cid; entity_id})
    | Get { cid; entity_id; key } -> handler (Ok {cid; entity_id}) 
    | Put { cid; access_id ; key; value } -> handler (Ok {cid; entity_id=access_id}) 
    | Patch { cid; access_id; key; value }  -> handler (Ok {cid; entity_id=access_id}) 
    | Remove { cid; access_id; key } -> handler (Ok {cid; entity_id=access_id}) 
    | Notify { cid; sid; values } -> handler (Ok {cid; entity_id=Auto}) 
    | Values { cid; values } -> handler (Ok {cid; entity_id=Auto}) 
    | Error  { cid; reason } -> handler (Ok {cid; entity_id=Auto}) 
    | Ok  { cid;  entity_id} -> handler (Ok {cid; entity_id=Auto})      

  let start e  =
    let open Lwt.Infix in 
    let rec loop () = 
      match%lwt EventStream.Source.get e.evt_src with
      | Some (EventWithHandler (msg, handler))  ->           
        (* let%lwt _ = Logs_lwt.debug (fun m -> m "Processing message id = %d " msg.cid) in  *)
        process e msg handler >>= loop

      | Some (Event msg)  ->           
        (* let%lwt _ = Logs_lwt.debug (fun m -> m "Processing message id = %d " msg.cid) in  *)
        process e msg (fun e -> Lwt.return_unit) >>= loop

      | None -> loop ()
    in loop ()

end

