open Yaks_core
open Yaks_event

module type Key = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val compare : t -> t-> int
  val prefix : t -> t -> bool
  val matches : t -> t -> bool
end


module type Value = sig
  type t
  val to_string : t -> string
  val of_string : string -> t
  val update : t -> t -> t
  val none : t
end

module type Store = sig
  type t
  type key
  type value
  type version

  val create: string -> int -> t
  (* val get : key -> t -> value option *)
  val get : key -> t -> (key * value) list option
  val remove  : key -> t -> t
  val put : key -> value -> t ->  t 
  val dput : key -> value -> t -> t
  val update_value : key -> value -> t -> t
  val keys : t -> key list
  val dump : t -> unit
  val empty : t 

end

type tuple = { key: string; value: Lwt_bytes.t }

type message = 
  | Create of { cid: int64; entity : entity; entity_id : entity_identifier }
  | Dispose of { cid: int64; entity_id : entity_identifier }
  | Get of { cid: int64; entity_id : entity_identifier; key : string; encoding: encoding option }
  | Put of { cid: int64; access_id : entity_identifier; key : string; value : string }
  | Patch of { cid: int64; access_id : entity_identifier; key : string; value : string }
  | Remove of { cid: int64; access_id : entity_identifier; key : string }
  | Notify of { cid: int64; sid : entity_identifier; values: tuple list }
  | Values of { cid: int64; encoding: encoding; values : tuple list }
  | Error of { cid : int64; reason : int }
  | Ok of { cid : int64;  entity_id: entity_identifier}

module MakeStore (K : Key) (Va : Value) = struct


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
(*   
  let get key store = MMap.find_opt key store.cache  *)

  let get_all key store = 
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
    ignore @@ Logs_lwt.debug "\n######\nSIZE: %d Current Size: %d\n" store.size (MMap.cardinal store.cache);
    ignore @@ MMap.iter (fun k va -> Logs_lwt.debug "K: %s Va: %s\n" (K.to_string k) (Va.to_string va)) store.cache;
    ignore @@ Logs_lwt.debug "######\n"

  
  let update_value key value store = {store with cache = MMap.add key value store.cache}

  let empty = {id = "0"; cache = MMap.empty ; size = 0}

end

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
    
    (* Sobstitute * with .* simplest matching *)
    
    
    (* let re_b = Str.regexp b in 
    (Str.string_match re_a b 0) || (Str.string_match re_b a 0) *)

end


module BValue = struct
type t = Lwt_bytes.t
  let of_string v = Lwt_bytes.of_string v
  let to_string s = Lwt_bytes.to_string s
  let update _ b = b
  let none = ""
end

module YAKSMM = MakeStore.Make(SKey)(BValue) 









  
   


   