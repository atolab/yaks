open Yaks_types

module Access = struct

  module Id = Apero.Uuid

  type t = 
    { id : Id.t 
    ; path : Path.t 
    ; cache_size : int64
    ; as_string : string 
    ; subs : Yaks_types.SubscriberId.t list }

  let make ?alias path cache_size =
    let uuid = match alias with | Some(a) -> Id.make_from_alias a | None -> Id.make () in
    { id = uuid; path; cache_size; as_string = "Acc#"^(Id.to_string uuid)^"("^(Path.to_string path)^")"; subs = [] }

  let id a = a.id
  let alias a = Id.alias a.id
  let path a = a.path 
  let cache_size a = a.cache_size

  let to_string a =a.as_string

  let is_covering a sel = Selector.is_prefixed_by_path a.path sel

  let register_subscriber a sid = {a with subs = sid::a.subs}
  let unregister_subscriber a sid  = { a with subs = List.filter (fun id -> sid != id) a.subs}
  let subscribers a = a.subs


end  [@@deriving show]

