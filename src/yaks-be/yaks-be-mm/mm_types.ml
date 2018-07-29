

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

  val create: string -> int -> t
  val get : key -> t -> (key * value) list option
  val remove  : key -> t -> t
  val put : key -> value -> t ->  t 
  val dput : key -> value -> t -> t
  val update_value : key -> value -> t -> t
  val keys : t -> key list
  val dump : t -> unit
  val empty : t 

end



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








