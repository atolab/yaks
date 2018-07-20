open Apero
open Yaks_event
open Actor

module Engine = struct 

  type t =  {
    my_mailbox: event Actor.actor_mailbox;
    my_loop: unit Lwt.t;
  }

  type config = { channel_len : int;}  

  type access = { path: string; cache_size: int64 }

  module PMap = Map.Make(String)

  type state = { access: access PMap.t; be_mailbox: event Actor.actor_mailbox; }


  let mailbox e = e.my_mailbox

  let generate_id () = Int64.to_string @@ Random.int64 Int64.max_int

  let create_access state ?id path cache_size =
    let id = Option.get_or_default id (generate_id ()) in
    id,
    { access = PMap.add id {path;cache_size} state.access; 
      be_mailbox = state.be_mailbox
    }

  let get_access state id = PMap.find id state.access

  let dispose_access state id =
    ignore @@ PMap.find id state.access;
    { access = PMap.remove id state.access;
      be_mailbox = state.be_mailbox }




  let push_to_be state msg =
    let%lwt _ = Logs_lwt.debug (fun m -> m "[ENG]   sending to BE: %s" (string_of_message msg)) in
    let (promise, resolver) = Lwt.task () in
    let on_reply = fun reply ->
      let%lwt _ = Logs_lwt.debug (fun m -> m "[ENG]   recv from BE %s" (string_of_message reply)) in
      Lwt.return (Lwt.wakeup_later resolver reply)
    in
    let open Actor in
    let%lwt _ = state.be_mailbox <!> (None, (EventWithHandler (msg, on_reply))) in
    promise

  let forward_to_be state msg handler =
    let open Lwt.Infix in 
    push_to_be state msg 
    >>= fun reply ->
    handler(reply) >|= fun _ -> state


  let process state (msg : message) (handler : message_handler) = 
    let%lwt _ = Logs_lwt.debug (fun m -> m "[ENG] recv from FE: %s" (string_of_message msg)) in
    let open Lwt.Infix in 
    match msg with
    | Create {cid; entity=Access{path; cache_size}; entity_id=AccessId(id)} ->
      (try
         let id,new_state = create_access state ~id path cache_size in
         handler (Ok {cid; entity_id=AccessId(id)}) >|= fun _ -> new_state
       with _ ->
         handler (Error {cid; reason=42}) >|= fun _ -> state
      )

    | Create {cid; entity=Access{path; cache_size}; entity_id=Auto} ->
      (try
         let id,new_state = create_access state path cache_size in
         handler (Ok {cid; entity_id=AccessId(id)}) >|= fun _ -> new_state
       with 
       | _ -> handler (Error {cid; reason=42}) >|= fun _ -> state
      )

    | Create {cid; entity=Storage{path; properties}; entity_id} ->
      forward_to_be state msg handler

    | Dispose { cid; entity_id=AccessId(id) }  ->
      (try
         let new_state = dispose_access state id in
         handler (Ok {cid; entity_id=AccessId(id)}) >|= fun _ -> new_state
       with 
       | Not_found -> handler (Error {cid; reason=404}) >|= fun _ -> state
       |_ -> handler (Error {cid; reason=42}) >|= fun _ -> state
      )

    | Dispose { cid; entity_id=StorageId(id) }  ->
      forward_to_be state msg handler

    | Get { cid; entity_id; key } ->
      forward_to_be state msg handler

    | Put { cid; access_id ; key; value } ->
      forward_to_be state msg handler

    | Patch { cid; access_id; key; value } ->
      forward_to_be state msg handler

    | Remove { cid; access_id; key } ->
      forward_to_be state msg handler

    | _ ->
      ignore @@ Logs_lwt.err (fun m -> m"[ENG] Received a unsupported or malformed message !!");
      handler (Error {cid=0L; reason=43}) >|= fun _ -> state



  let create cfg be_mailbox = 
    let init_state = { access = PMap.empty; be_mailbox } in
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




