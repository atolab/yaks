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
  module SubscriptionMap = Map.Make(Int64)

  type state = { access: access PMap.t; be_mailbox: event Actor.actor_mailbox; subscriptions: (string * (event Actor.actor_mailbox)) SubscriptionMap.t }
  (* type state = { access: access PMap.t; be_mailbox: event Actor.actor_mailbox; subscriptions: string SubscriptionMap.t } *)

  let mailbox e = e.my_mailbox

  let generate_id () = Int64.to_string @@ Random.int64 Int64.max_int

  let create_access (state:state) ?id path cache_size =
    let id = Option.get_or_default id (generate_id ()) in
    id,
    { access = PMap.add id {path;cache_size} state.access; 
      be_mailbox = state.be_mailbox;
      subscriptions = state.subscriptions
    }

  let get_access (state:state) id = PMap.find id state.access

  let dispose_access (state:state) id =
    ignore @@ PMap.find id state.access;
    { access = PMap.remove id state.access;
      be_mailbox = state.be_mailbox;
      subscriptions = state.subscriptions }

  (* let add_subscriber ?id key state =  *)
  let add_subscriber ?id key mailbox (state:state) = 
    let id = Option.get_or_default id (Random.int64 Int64.max_int) in
    id,{state with subscriptions = SubscriptionMap.add id (key,mailbox ) state.subscriptions}
    (* id,{state with subscriptions = SubscriptionMap.add id key state.subscriptions} *)

  let remove_subscriber id (state:state) = 
    id,{state with subscriptions = SubscriptionMap.remove id state.subscriptions}

  let get_subscribers (key:string) (state:state) = 
    
    (* let x = SubscriptionMap.bindings state.subscriptions |> 
    (* TODO this should use the Ypath module with some function like Ypath.matches -> t -> t -> bool *)
    List.filter (fun (p,mb) -> p=key)
    in *)
    (* List.iter (fun (x,y) -> Printf.printf "X %Ld" x); *)
    (* List.map (fun (_,(_,mb)) -> mb ) l *)
    []


  let push_to_be (state:state) (msg : message)  =
    let%lwt _ = Logs_lwt.debug (fun m -> m "[ENG]   sending to BE: %s" (string_of_message msg)) in
    let (promise, resolver) = Lwt.task () in
    let on_reply = fun reply ->
      let%lwt _ = Logs_lwt.debug (fun m -> m "[ENG]   recv from BE %s" (string_of_message reply)) in
      Lwt.return (Lwt.wakeup_later resolver reply)
    in
    let open Actor in
    let%lwt _ = state.be_mailbox <!> (None, (EventWithHandler (msg, on_reply))) in
    promise

  let forward_to_be (state:state) (msg : message) (handler : message_handler)  =
    let open Lwt.Infix in 
    push_to_be state msg 
    >>= fun reply ->
    handler(reply) >|= fun _ -> state

  let process (state:state) (msg : message) (handler : message_handler) (from: event Actor.actor_mailbox option) = 
  (* let process state (msg : message) (handler : message_handler) =  *)
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
      let r = forward_to_be state msg handler in
      let values = [{ key; value=(Lwt_bytes.of_string value)}] in
      let subscribers = get_subscribers key state in
      List.iter (fun e -> let msg = Values{cid; encoding = `String; values} in let _ = e <!> (None,Event (msg)) in ()) subscribers;
      r

    | Patch { cid; access_id; key; value } ->
      forward_to_be state msg handler

    | Remove { cid; access_id; key } ->
      forward_to_be state msg handler

    | Subscribe { cid; entity_id=SubscriberId(sid); key } ->

      let msg,new_state = 
        match from with 
          | Some mb -> 
            let id,ns = add_subscriber ~id:sid key mb state in
            (Ok {cid; entity_id=SubscriberId(id)}),ns
          | None ->  
            ignore @@ Logs_lwt.err (fun m -> m"[ENG] Observer should have a mailbox to send notifications !!");
            (Error {cid; reason=44}),state
      in
      (* let id,new_state = add_subscriber ~id:sid key state in *)
      handler msg >|= fun _ -> new_state

    | Unsubscribe {cid; entity_id=SubscriberId(sid)} ->
      let id,new_state = remove_subscriber sid state in
      handler (Ok {cid; entity_id=SubscriberId(id)}) >|= fun _ -> new_state
    
    | _ ->
      ignore @@ Logs_lwt.err (fun m -> m"[ENG] Received a unsupported or malformed message !!");
      handler (Error {cid=0L; reason=43}) >|= fun _ -> state



  let create cfg be_mailbox = 
    let init_state = { access = PMap.empty; be_mailbox; subscriptions = SubscriptionMap.empty } in
    let open Actor in
    let open Lwt.Infix in 
    let my_mailbox,my_loop = spawn ~state:(Some init_state) (fun self state from ->
        let current_state =
          match state with
          | Some(s) -> s
          | None -> failwith "!!!!! Error state must be present" in
        function
        | EventWithHandler (msg, handler)  ->
          process current_state msg handler from
          >>= fun new_state -> continue self (Some(new_state)) ()
        | Event (msg) ->
          process current_state msg (fun e -> Lwt.return_unit) from
          (* Sending passing the mailbox to process because in the case of subscription I need the mailbox of the actor that is handling that subscription *)
          >>= fun new_state -> continue self (Some(new_state)) ()
      )
    in { my_mailbox; my_loop }


  let start e = e.my_loop

end




