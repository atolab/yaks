open Yaks_event
open Actor

module Engine = struct 

  type t =  { my_mailbox: event Actor.actor_mailbox; my_loop: unit Lwt.t }

  type config = { channel_len : int;}  

  type state = { access: int list }


  let mailbox e = e.my_mailbox

  let process state (msg : message) (handler : message_handler) = 
    let open Lwt.Infix in 
    match msg with 
    | Create {cid; entity; entity_id} ->
      let new_state = { access = 3::state.access } in
      handler (Ok {cid; entity_id}) >|= fun _ -> new_state
    | Dispose { cid; entity_id }  ->
      handler (Ok {cid; entity_id}) >|= fun _ -> state
    | Get { cid; entity_id; key } ->
      handler (Ok {cid; entity_id}) >|= fun _ -> state
    | Put { cid; access_id ; key; value } ->
      handler (Ok {cid; entity_id=access_id}) >|= fun _ -> state
    | Patch { cid; access_id; key; value } ->
      handler (Ok {cid; entity_id=access_id}) >|= fun _ -> state
    | Remove { cid; access_id; key } ->
      handler (Ok {cid; entity_id=access_id}) >|= fun _ -> state
    | Notify { cid; sid; values } ->
      handler (Ok {cid; entity_id=Auto}) >|= fun _ -> state
    | Values { cid; values } ->
      handler (Ok {cid; entity_id=Auto}) >|= fun _ -> state
    | Error  { cid; reason } ->
      handler (Ok {cid; entity_id=Auto}) >|= fun _ -> state
    | Ok  { cid;  entity_id} ->
      handler (Ok {cid; entity_id=Auto}) >|= fun _ -> state



  let create cfg = 
    let init_state = { access = [] } in
    let open Actor in
    let open Lwt.Infix in 
    let my_mailbox,my_loop = spawn ~state:(Some init_state) (fun self state from ->
        let current_state =
          match state with
          | Some(s) -> s
          | None -> failwith "!!!!! Error state must be present" in
        function
        | EventWithHandler (msg, handler)  ->
          process current_state msg handler
          >>= fun new_state -> continue self (Some(new_state)) true
        | Event (msg) ->
          process current_state msg (fun e -> Lwt.return_unit)
          >>= fun new_state -> continue self (Some(new_state)) true
      )
    in { my_mailbox; my_loop }


  let start e = e.my_loop

end




