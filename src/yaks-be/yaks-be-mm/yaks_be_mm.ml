open Yaks_core
open Yaks_event
open Types_signatures
open Mm_types
open Lwt.Infix


module SKey =  struct
  type t = string
  let of_string s = s
  let to_string s = s
  let compare = String.compare
  let prefix a b =
    let len1 = String.length b
    and len2 = String.length a in
    if len1 < len2 then false else
      let rec aux i =
        if i < 0 then false else
          let sub = String.sub b i len2 in
          if (sub = a) then true else aux (pred i)
      in
      aux (len1 - len2)
  let matches a b =
    let splitted = (String.split_on_char '*' a) in
    let len = List.length splitted in
    match len with
    | 0 -> false
    | 1 -> 
      let a_s = List.hd splitted ^ ".*" in 
      let re_a = Str.regexp a_s in (Str.string_match re_a b 0)
    | 2 -> 
      let a_s = (List.hd splitted) ^ ".*" ^ (List.nth  splitted (len-1) ) in
      let re_a = Str.regexp a_s in (Str.string_match re_a b 0)
    | _ -> false
end

module BValue = struct
  type t = Lwt_bytes.t
  let of_string v = Lwt_bytes.of_string v
  let to_string s = Lwt_bytes.to_string s
  let update _ b = b
  let none = Lwt_bytes.create 0 
end

module MemStore = struct

  module K = SKey
  module Va = BValue

  type key = K.t
  type value = Va.t

  module MMap = Map.Make(K)

  type t = { 
    id : string;
    cache : (value) MMap.t;
    size: int; 
  }

  let create id size = 
    {id = id; cache = MMap.empty ; size = size}

  let get key store = 
    let f_map = MMap.filter (fun k _ -> K.matches key k) store.cache in
    let b = MMap.bindings f_map in 
    let vss = List.map (fun (k,va) -> k,va) b in Some(vss)

  let remove key store = {store with cache = MMap.remove key store.cache}

  let put key value store = 
    match MMap.mem key store.cache with
    | true ->  {store with cache = MMap.add key value store.cache}
    | false -> {store with cache = MMap.add key value store.cache}


  let dput key value store = 
    match MMap.mem key store.cache with
    | true -> 
      let old_value = MMap.find key store.cache in
      let new_value = Va.update old_value value in
      {store with cache = MMap.add key new_value store.cache}
    | false -> {store with cache = MMap.add key value store.cache}

  let keys store = 
    List.map (fun (k, _) -> k) (MMap.bindings store.cache)

  let dump store =
    ignore @@ Logs_lwt.debug (fun m -> m "\n######\nSIZE: %d Current Size: %d\n" store.size (MMap.cardinal store.cache));
    ignore @@ MMap.iter (fun k va -> ignore @@ Logs_lwt.debug (fun m -> m "K: %s Va: %s\n" (K.to_string k) (Va.to_string va))) store.cache;
    ignore @@ Logs_lwt.debug (fun m -> m "######\n")


  let update_value key value store = {store with cache = MMap.add key value store.cache}

  let empty = {id = "0"; cache = MMap.empty ; size = 0}

end

module StoreMap = Map.Make(struct type t = string let compare = compare end)
module PrefixMap = Map.Make(struct type t = string let compare = compare end)

type t = {
  cfg: string;
  stores : (MemStore.t) StoreMap.t;
  prefix_map : (string) PrefixMap.t
}


(* HELPER FUNCTIONS *)

let create_store sid path mm_be = 
  let s = MemStore.create sid 1024 in
  let pm = PrefixMap.add path sid mm_be.prefix_map in 
  let sm = StoreMap.add sid s mm_be.stores in 
  { mm_be with stores = sm; prefix_map = pm} 


let get_path_by_id sid mm_be = 
  let l = PrefixMap.bindings mm_be.prefix_map in
  List.filter (fun (_,v) -> v=sid) l

let get_sid_by_path path mm_be = 
  PrefixMap.find_opt path mm_be.prefix_map


let find_matching_storages path mm_be = 
  let l = PrefixMap.bindings mm_be.prefix_map in 
  List.filter (fun (k,v) -> SKey.prefix k path) l


let dispose_store sid mm_be = 
  let p,_ = 
    match List.nth_opt (get_path_by_id sid mm_be) 0 with
    | Some s-> s
    | None -> failwith @@ Printf.sprintf "MainMemory-BE error! Unable to find path for stogare id %s" sid
  in
  {mm_be with stores = StoreMap.remove sid mm_be.stores; prefix_map = PrefixMap.remove p mm_be.prefix_map}

let check_if_storage_exists sid mm_be = 
  StoreMap.mem sid mm_be.stores

let update_storage sid storage mm_be = 
  { mm_be with stores = StoreMap.add sid storage mm_be.stores} 

let get_state state =
  match state with 
  | Some s -> s
  | None -> 
    ignore @@ Logs_lwt.debug (fun m -> m"[MM] State is not present! This is an error!");
    failwith "Error state must be present"



(* ACTOR *)

open Actor.Actor
let process current_state (msg : message) (handler : message_handler) = 
  let open Lwt.Infix in 
  match msg with 
  | Create cmsg ->
    ignore @@ Logs_lwt.debug (fun m -> m"[MM] Received create");
    let reponse, new_state = 
      match cmsg.entity with
      | Storage storage -> 
        let path = storage.path in
        (
          match cmsg.entity_id with
          | StorageId s -> 
            ignore @@ Logs_lwt.debug (fun m -> m "[MM] Creating storage with ID %s and path %s" s path);
            let ns = create_store s path current_state in
            Ok{cid = cmsg.cid; entity_id = StorageId s }, ns
          | _ -> 
            ignore @@ Logs_lwt.debug (fun m -> m"[MM] Wrong formatted message, entity_identifier is not StorageId");
            Error {cid = cmsg.cid; reason = (-1) }, current_state
        )
      | _ -> 
        ignore @@ Logs_lwt.debug (fun m -> m"[MM] Wrong formatted message, entity is not Storage");
        Error {cid = cmsg.cid; reason = (-1) }, current_state
    in
    handler (reponse) >|= fun _ -> new_state

  | Dispose dmsg ->
    ignore @@ Logs_lwt.debug (fun m -> m"[MM] Received dispose");
    let reponse,new_state = 
      match dmsg.entity_id with
      | StorageId s -> 
        ignore @@ Logs_lwt.debug (fun m -> m"[MM] Disposing storage with ID %s" s);
        let ns = dispose_store s current_state in
        Ok {cid = dmsg.cid; entity_id = StorageId s }, ns
      | _ -> 
        ignore @@ Logs_lwt.debug (fun m -> m"[MM] Wrong formatted message, entity_identifier is not StorageId");
        Error {cid = dmsg.cid; reason = (-1) }, current_state
    in
    handler (reponse) >|= fun _ -> new_state

  | Get gmsg ->
    ignore @@ Logs_lwt.debug (fun m -> m"[MM] Received get");
    let matching_storages = find_matching_storages gmsg.key current_state in
    let reponse,new_state = 
      match List.length matching_storages with
      | 0 -> 
        ignore @@ Logs_lwt.debug (fun m -> m"[MM] No storage have resposability for this key");
        Error {cid = gmsg.cid; reason = (-2) }, current_state
      | _ ->
        ignore @@ Logs_lwt.debug (fun m -> m"[MM] Getting from all storage that have responsability for this key: %s" gmsg.key);
        let values = 
          let storages = List.map (fun (_,id) -> StoreMap.find id current_state.stores) matching_storages in
          List.flatten @@ List.map (fun e -> 
              match MemStore.get gmsg.key e with
              | Some vs -> List.map (fun (k,v) -> { key = SKey.to_string k; value = v }) vs
              | None -> []
            ) storages
        in
        Values{cid = gmsg.cid; encoding = `String ; values }, current_state
        (* match gmsg.entity_id with
           | StorageId s -> 
           ignore @@ Logs_lwt.debug (fun m -> m"[MM] Getting with ID %s checking if storage exists" s);
           match check_if_storage_exists s current_state with
           | true -> 
            ignore @@ Logs_lwt.debug (fun m -> m"[MM] Storage ID %s exists" s);
            let storage = StoreMap.find s current_state.stores in
            let values = 
              match MemStore.get gmsg.key storage with
              | Some vs -> List.map (fun (k,v) -> { key = SKey.to_string k; value = v }) vs
              | None -> []
            in
            Values{cid = gmsg.cid; encoding = `String ; values }, current_state
           | false -> 
            ignore @@ Logs_lwt.debug (fun m -> m"[MM] Storage with ID %s does not exists!" s);
            Error {cid = gmsg.cid; reason = (-2) }, current_state
           | _ -> 
           ignore @@ Logs_lwt.debug (fun m -> m"[MM] Wrong formatted message, entity_identifier is not StorageId");
           Error {cid = gmsg.cid; reason = (-1) }, current_state *)
    in
    handler (reponse) >|= fun _ -> new_state

  | Put put_msg ->
    ignore @@ Logs_lwt.debug (fun m -> m"[MM] Received Put");
    let matching_storages = find_matching_storages put_msg.key current_state in
    let reponse,new_state = 
      match List.length matching_storages with
      | 0 -> 
        ignore @@ Logs_lwt.debug (fun m -> m"[MM] No storage have resposability for this key");
        Error {cid = put_msg.cid; reason = (-2) }, current_state
      | _ ->
        ignore @@ Logs_lwt.debug (fun m -> m"[MM] Storing in all storage that have responsability for this key: %s" put_msg.key);
        let new_storages = 
          let storages = List.map (fun (_,id) -> id, StoreMap.find id current_state.stores) matching_storages in
          List.map (fun (id,e) -> id, MemStore.put put_msg.key (BValue.of_string put_msg.value) e) storages
        in
        let rec update_state storage_list i current_state = 
          if i < List.length storage_list then 
            let id,storage = (List.nth storage_list i) in
            update_state storage_list (i+1) (update_storage id storage current_state)
          else
            current_state
        in 
        let ns = update_state new_storages 0 current_state in
        Ok{cid = put_msg.cid; entity_id = StorageId ""}, ns
        (* TODO storage id is? The real storage id or the id of the backend? *)
        (* let reponse,new_state = 
           match put_msg.access_id with
           | StorageId s -> 
            ignore @@ Logs_lwt.debug (fun m -> m"[MM] Putting with ID %s checking if storage exists" s);
            match check_if_storage_exists s current_state with
            | true -> 
              ignore @@ Logs_lwt.debug (fun m -> m"[MM] Storage ID %s exists" s);
              let storage = StoreMap.find s current_state.stores in
              let new_storage = MemStore.put put_msg.key (BValue.of_string put_msg.value) storage in
              let ns = update_storage s new_storage current_state in
              Ok{cid = put_msg.cid; entity_id = StorageId s}, ns
            | false -> 
              ignore @@ Logs_lwt.debug (fun m -> m"[MM] Storage with ID %s does not exists!" s);
              Error {cid = put_msg.cid; reason = (-2) }, current_state
           | _ -> 
            ignore @@ Logs_lwt.debug (fun m -> m"[MM] Wrong formatted message, entity_identifier is not StorageId");
            Error {cid = put_msg.cid; reason = (-1) }, current_state *)
    in
    handler (reponse) >|= fun _ -> new_state

  | Patch patch_msg ->
    ignore @@ Logs_lwt.debug (fun m -> m"[MM] Received Patch");
    let matching_storages = find_matching_storages patch_msg.key current_state in
    let reponse,new_state = 
      match List.length matching_storages with
      | 0 -> 
        ignore @@ Logs_lwt.debug (fun m -> m"[MM] No storage have resposability for this key");
        Error {cid = patch_msg.cid; reason = (-2) }, current_state
      | _ ->
        ignore @@ Logs_lwt.debug (fun m -> m"[MM] Storing in all storage that have responsability for this key: %s" patch_msg.key);
        let new_storages = 
          let storages = List.map (fun (_,id) -> id,StoreMap.find id current_state.stores) matching_storages in
          List.map (fun (id,e) -> id, MemStore.dput patch_msg.key (BValue.of_string patch_msg.value) e ) storages
        in
        let rec update_state storage_list i current_state = 
          if i < List.length storage_list then
            let id,storage = (List.nth storage_list i) in
            update_state storage_list (i+1) (update_storage id storage current_state)
          else
            current_state
        in 
        let ns = update_state new_storages 0 current_state in
        Ok{cid = patch_msg.cid; entity_id = StorageId ""}, ns
        (* let reponse,new_state = 
           match patch_msg.access_id with
           | StorageId s -> 
            ignore @@ Logs_lwt.debug (fun m -> m"[MM] Patching with ID %s checking if storage exists" s);
            match check_if_storage_exists s current_state with
            | true -> 
              ignore @@ Logs_lwt.debug (fun m -> m"[MM] Storage ID %s exists" s);
              let storage = StoreMap.find s current_state.stores in
              let new_storage = MemStore.dput patch_msg.key (BValue.of_string patch_msg.value) storage in
              let ns = update_storage s new_storage current_state in
              Ok{cid = patch_msg.cid; entity_id = StorageId s}, ns
            | false -> 
              ignore @@ Logs_lwt.debug (fun m -> m"[MM] Storage with ID %s does not exists!" s);
              Error {cid = patch_msg.cid; reason = (-2) }, current_state
           | _ -> 
           ignore @@ Logs_lwt.debug (fun m -> m"[MM] Wrong formatted message, entity_identifier is not StorageId");
           Error {cid = patch_msg.cid; reason = (-1) }, current_state *)
    in
    handler (reponse) >|= fun _ -> new_state

  | Remove rmsg ->
    ignore @@ Logs_lwt.debug (fun m -> m"[MM] Received Patch");
    let matching_storages = find_matching_storages rmsg.key current_state in
    let reponse,new_state = 
      match List.length matching_storages with
      | 0 -> 
        ignore @@ Logs_lwt.debug (fun m -> m"[MM] No storage have resposability for this key");
        Error {cid = rmsg.cid; reason = (-2) }, current_state
      | _ ->
        ignore @@ Logs_lwt.debug (fun m -> m"[MM] Storing in all storage that have responsability for this key: %s" rmsg.key);
        let new_storages = 
          let storages = List.map (fun (_,id) -> id, StoreMap.find id current_state.stores) matching_storages in
          List.map (fun (id,e) -> id, MemStore.remove rmsg.key e ) storages
        in
        let rec update_state storage_list i current_state = 
          if i < List.length storage_list then
            let id,storage = (List.nth storage_list i) in
            update_state storage_list (i+1) (update_storage id storage current_state)
          else
            current_state
        in 
        let ns = update_state new_storages 0 current_state in
        Ok{cid = rmsg.cid; entity_id = StorageId ""}, ns
        (* let reponse,new_state = 
           match rmsg.access_id with
           | StorageId s -> 
            ignore @@ Logs_lwt.debug (fun m -> m"[MM] Patching with ID %s checking if storage exists" s);
            match check_if_storage_exists s current_state with
            | true -> 
              ignore @@ Logs_lwt.debug (fun m -> m"[MM] Storage ID %s exists" s);
              let storage = StoreMap.find s current_state.stores in
              let new_storage = MemStore.remove rmsg.key storage in
              let ns = update_storage s new_storage current_state in
              Ok{cid = rmsg.cid; entity_id = StorageId s}, ns
            | false -> 
              ignore @@ Logs_lwt.debug (fun m -> m"[MM] Storage with ID %s does not exists!" s);
              Error {cid = rmsg.cid; reason = (-2) }, current_state
           | _ -> 
            ignore @@ Logs_lwt.debug (fun m -> m"[MM] Wrong formatted message, entity_identifier is not StorageId");
            Error {cid = rmsg.cid; reason = (-1) }, current_state *)
    in
    handler (reponse) >|= fun _ -> new_state

  | Notify nmsg ->
    ignore @@ Logs_lwt.debug (fun m -> m"[MM] Received Notify doing nothing");
    handler (Error {cid = nmsg.cid; reason = (-42) }) >|= fun _ -> current_state
  | Values vmsg ->
    ignore @@ Logs_lwt.debug (fun m -> m"[MM] Received Values doing nothing");
    handler (Error {cid = vmsg.cid; reason = (-42) }) >|= fun _ -> current_state
  | Ok ok_msg ->
    ignore @@ Logs_lwt.debug (fun m -> m"[MM] Received Ok doing nothing");
    handler (Error {cid = ok_msg.cid; reason = (-42) }) >|= fun _ -> current_state
  | Error err ->
    ignore @@ Logs_lwt.debug (fun m -> m"[MM] Received Error doing nothing");
    handler (Error {cid = err.cid; reason = (-42) }) >|= fun _ -> current_state



let create cfg = 
  ignore @@ Logs_lwt.debug (fun m -> m"MainMemory-BE Creating stores map");
  let init_state = {prefix_map = PrefixMap.empty ;stores = StoreMap.empty; cfg} in
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
        >>= fun new_state -> continue self (Some(new_state)) ()
      | Event (msg) ->
        process current_state msg (fun e -> Lwt.return_unit)
        >>= fun new_state -> continue self (Some(new_state)) ()
    )
  in my_mailbox,my_loop
