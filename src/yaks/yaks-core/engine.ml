open Apero
open Yaks_event
open Actor.Infix
open Lwt.Infix

module Engine = struct 

  type t =  {
    my_mailbox: event Actor.actor_mailbox;
    my_loop: unit Lwt.t;
  }

  type config = { channel_len : int;}  

  type access = { path: string; cache_size: int64 }

  module PMap = Map.Make(String)
  module SubscriptionMap = Map.Make(Int64)
  module BEMap = Map.Make(Int64)
  module FEMap = Map.Make(Int64)
  module TXMap = Map.Make(Int64)

  type state = { 
    access: access PMap.t; 
    (* be_mailbox: event Actor.actor_mailbox;  *)
    subscriptions: (string * (event Actor.actor_mailbox)) SubscriptionMap.t;
    backends: (event Actor.actor_mailbox) BEMap.t;
    frontends : (event Actor.actor_mailbox) FEMap.t;
    transports : (event Actor.actor_mailbox) TXMap.t;
    }

  let mailbox e = e.my_mailbox

  let generate_id () = Int64.to_string @@ Random.int64 Int64.max_int


  let add_plugin plugin_mb id kind state = 
  let r,s = 
  match kind with
  | Backend -> 
    let r,s = match BEMap.mem id state.backends with
    | true -> 
      ignore @@ Logs_lwt.err (fun m -> m"[ENG] Backend with id %Ld already exists !!" id);
      false,state
    | false -> 
      ignore @@ Logs_lwt.debug (fun m -> m "[ENG] Adding backend %Ld" id);
      true,{state with backends = BEMap.add id plugin_mb state.backends}
    in r,s
  | Frontend -> 
    let r,s = match FEMap.mem id state.frontends with
    | true -> 
      ignore @@ Logs_lwt.err (fun m -> m"[ENG] Frontend with id %Ld already exists !!" id);
      false,state
    | false -> 
      ignore @@ Logs_lwt.debug (fun m -> m "[ENG] Adding Frontend %Ld" id);
      true,{state with frontends = FEMap.add id plugin_mb state.frontends}
     in r,s
  | Transport -> 
    let r,s = match TXMap.mem id state.transports with
    | true -> 
      ignore @@ Logs_lwt.err (fun m -> m"[ENG] Transport with id %Ld already exists !!" id);
      false,state
    | false -> 
      ignore @@ Logs_lwt.debug (fun m -> m "[ENG] Adding Transport %Ld" id);
      true,{state with transports = TXMap.add id plugin_mb state.transports}
     in r,s
  | _ -> 
    ignore @@ Logs_lwt.err (fun m -> m"[ENG] Unknow plugin type !!");
    false,state
  in r,s

  let remove_plugin id kind state = 
  let r,s = 
  match kind with
  | Backend -> 
    let r,s = match BEMap.mem id state.backends with
    | true -> 
      ignore @@ Logs_lwt.debug (fun m -> m "[ENG] Removing Backend %Ld" id);
      true,{state with backends = BEMap.remove id state.backends}
    | false -> 
      ignore @@ Logs_lwt.err (fun m -> m"[ENG] Backend with id %Ld not exists !!" id);
      false,state
     in r,s
  | Frontend -> 
    let r,s = match FEMap.mem id state.frontends with
    | true -> 
      ignore @@ Logs_lwt.debug (fun m -> m "[ENG] Removing Frontend %Ld" id);
      true,{state with frontends = FEMap.remove id state.frontends}
    | false -> 
      ignore @@ Logs_lwt.err (fun m -> m"[ENG] Frontend with id %Ld not exists !!" id);
       false,state
    in r,s
  | Transport -> 
    let r,s = match TXMap.mem id state.transports with
    | true -> 
      ignore @@ Logs_lwt.debug (fun m -> m "[ENG] Removing Transport %Ld" id);
      true,{state with transports = FEMap.remove id state.transports}
    | false -> 
      ignore @@ Logs_lwt.err (fun m -> m"[ENG] Transport with id %Ld not exists !!" id);
       false,state
    in r,s
  | _ -> 
    ignore @@ Logs_lwt.err (fun m -> m"[ENG] Unknow plugin type !!");
    false,state
  in r,s

  let create_access ?id path cache_size state =
    let id = Option.get_or_default id (generate_id ()) in
    id,
    { state with access = PMap.add id {path;cache_size} state.access }

  let get_access id state = PMap.find id state.access

  let dispose_access  id state =
    ignore @@ PMap.find id state.access;
    { state with access = PMap.remove id state.access}

  (* let add_subscriber ?id key state =  *)
  let add_subscriber ?id key mailbox state= 
    let id = Option.get_or_default id (Random.int64 Int64.max_int) in
    id,{state with subscriptions = SubscriptionMap.add id (key,mailbox ) state.subscriptions}

  let remove_subscriber id state = 
    id,{state with subscriptions = SubscriptionMap.remove id state.subscriptions}

  let get_subscribers key state = 
    
    let xs = SubscriptionMap.bindings state.subscriptions in 
    let f = List.map (fun (k,x) -> x) xs in
    let ff = List.filter (fun (p,_) -> p=key) f in
    List.map (fun (_,mb) -> mb ) ff

  let push_to_be msg state  =
    ignore @@ Logs_lwt.debug (fun m -> m "[ENG]   sending to BE: %s" (string_of_message msg));
    let (promise, resolver) = Lwt.task () in
    let on_reply = fun reply ->
      ignore @@ Logs_lwt.debug (fun m -> m "[ENG]   recv from BE %s" (string_of_message reply));
      Lwt.return (Lwt.wakeup_later resolver reply)
    in
    (* let%lwt lb =  in *)
    let%lwt _ = Lwt_list.filter_p (fun (id,mb) -> Lwt.return true ) (BEMap.bindings state.backends) >>=
      Lwt_list.iter_p (fun (id,mb) -> Actor.send mb None (EventWithHandler (msg, on_reply))) in
    promise

  let forward_to_be msg handler state  =
    push_to_be msg state 
    >>= fun reply ->
    handler(reply) >|= fun _ -> state


  let push_to_tx msg from state =
    ignore @@ Logs_lwt.debug (fun m -> m "[ENG]   sending to TX: %s" (string_of_message msg));
    let (promise, resolver) = Lwt.task () in
    let on_reply = fun reply ->
      ignore @@ Logs_lwt.debug (fun m -> m "[ENG]   recv from TX %s" (string_of_message reply));
      Lwt.return (Lwt.wakeup_later resolver reply)
    in

    let%lwt _ = 
      ( match from with
        | Some s ->  Lwt_list.filter_p (fun (id,mb) -> Lwt.return (Actor.compare s mb <> 0) ) (TXMap.bindings state.transports)
        | None -> Lwt.return (TXMap.bindings state.transports)
      ) >>= Lwt_list.iter_p (fun (id,mb) -> Actor.send mb None (EventWithHandler (msg, on_reply))) 
    in    
    promise

  let forward_to_tx msg handler from state  =
    push_to_tx msg from state 
    >>= fun reply ->
    handler(reply) >|= fun _ -> state

  let process  msg handler from state = 
  (* let process state (msg : message) (handler : message_handler) =  *)
    ignore @@ Logs_lwt.debug (fun m -> m "[ENG] recv from FE: %s" (string_of_message msg));
    match msg with
    | Create {cid; entity=Access{path; cache_size}; entity_id=AccessId(id)} ->
      (try
         let id,new_state = create_access ~id path cache_size state in
         handler (Ok {cid; entity_id=AccessId(id)}) >|= fun _ -> new_state
       with _ ->
         handler (Error {cid; reason=42}) >|= fun _ -> state
      )

    | Create {cid; entity=Access{path; cache_size}; entity_id=Auto} ->
      (try
         let id,new_state = create_access path cache_size state in
         handler (Ok {cid; entity_id=AccessId(id)}) >|= fun _ -> new_state
       with 
       | _ -> handler (Error {cid; reason=42}) >|= fun _ -> state
      )

    | Create {cid; entity=Storage{path; properties}; entity_id} ->
      forward_to_be msg handler state

    | Dispose { cid; entity_id=AccessId(id) }  ->
      (try
         let new_state = dispose_access id state in
         handler (Ok {cid; entity_id=AccessId(id)}) >|= fun _ -> new_state
       with 
       | Not_found -> handler (Error {cid; reason=404}) >|= fun _ -> state
       |_ -> handler (Error {cid; reason=42}) >|= fun _ -> state
      )

    | Dispose { cid; entity_id=StorageId(id) }  ->
      forward_to_be msg handler state

    | Get { cid; entity_id; key } ->
      forward_to_be msg handler state

    | Put { cid; access_id ; key; value } ->
      (* let r = forward_to_be msg handler state >>= forward_to_tx msg handler from in  *)
      let r = forward_to_be msg handler state in
      let _ = forward_to_tx msg handler from state in 
      let values = [{ key; value=(Lwt_bytes.of_string value)}] in
      let subscribers = get_subscribers key state in
      List.iter (fun e -> let msg = Values{cid; encoding = `String; values} in let _ = Actor.send e None (Event (msg)) in ()) subscribers;
      r

    | Patch { cid; access_id; key; value } ->
      forward_to_be msg handler state >>= forward_to_tx msg handler from

    | Remove { cid; access_id; key } ->
      forward_to_be msg handler state >>= forward_to_tx msg handler from

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
      handler msg >|= fun _ -> state

    | Unsubscribe {cid; entity_id=SubscriberId(sid)} ->
      let id,new_state = remove_subscriber sid state in
      handler (Ok {cid; entity_id=SubscriberId(id)}) >|= fun _ -> new_state
    | AddPlugin {entity=Plugin(e);  entity_id=PluginId(id)} -> 
      let r,ns = add_plugin e.mailbox id e.kind state in 
      let msg,s = match r with
      | true -> (Ok {cid=0L; entity_id=PluginId(id)}),ns
      | false -> (Error {cid=0L; reason=45}),state
      in
      handler msg >|= fun _ -> s

    | RemovePlugin {entity_id=PluginId(id); kind} -> 
      let r,ns = remove_plugin id kind state in 
      let msg,s = match r with
      | true -> (Ok {cid=0L; entity_id=PluginId(id)}),ns
      | false -> (Error {cid=0L; reason=45}),state
      in
      handler msg >|= fun _ -> s
    | _ ->
      ignore @@ Logs_lwt.err (fun m -> m"[ENG] Received a unsupported or malformed message !!");
      handler (Error {cid=0L; reason=43}) >|= fun _ -> state



  let create cfg = 
    let init_state = 
    { 
      access = PMap.empty;
      (* be_mailbox; *)
      subscriptions = SubscriptionMap.empty;
      backends = BEMap.empty;
      frontends = FEMap.empty;
      transports = TXMap.empty;
      } 
    in
    let open Actor in
    let my_mailbox,my_loop = spawn ~state:(Some init_state) (fun self state from ->
        let current_state =
          match state with
          | Some(s) -> s
          | None -> failwith "!!!!! Error state must be present" in
        function
        | EventWithHandler (msg, handler)  ->
          process  msg handler from current_state
          >>= fun new_state -> continue self (Some(new_state)) ()
        | Event (msg) ->
          process msg (fun e -> Lwt.return_unit) from current_state
          (* Sending passing the mailbox to process because in the case of subscription I need the mailbox of the actor that is handling that subscription *)
          >>= fun new_state -> continue self (Some(new_state)) ()
      )
    in { my_mailbox; my_loop }


  let start e = e.my_loop

end




