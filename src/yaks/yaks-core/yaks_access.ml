open Apero
open Yaks_types
open Yaks_core_properties

module Access = struct

  module Id = Apero.Uuid

  type t = 
    { id : Id.t 
    ; path : Path.t 
    ; cache_size : int64
    ; as_string : string 
    ; subs : Yaks_types.SubscriberId.t list }

  let default_cache_size = 1024L  (* TODO: make this configurable *)

  let make path properties =
    let cache_size = Option.get_or_default 
      (Properties.decode_property_value Int64.of_string Property.Access.Key.cache_size properties)
      default_cache_size
    in 
    let alias = Properties.get Property.Access.Key.alias properties in 
    let uuid = match alias with | Some(a) -> Id.make_from_alias a | None -> Id.make () in
    { id = uuid; path; cache_size; as_string = "Acc#"^(Id.to_string uuid)^"("^(Path.to_string path)^")"; subs = [] }

  let id a = a.id
  let alias a = Id.alias a.id
  let path a = a.path 
  let cache_size a = a.cache_size

  let to_string a =a.as_string

  let is_covering_path a path = Path.is_prefix ~affix:a.path path
  let is_covering_selector a sel = Selector.is_prefixed_by_path a.path sel

  let register_subscriber a sid = {a with subs = sid::a.subs}
  let unregister_subscriber a sid  = { a with subs = List.filter (fun id -> sid != id) a.subs}
  let subscribers a = a.subs


end  [@@deriving show]
