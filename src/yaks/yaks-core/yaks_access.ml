open Yaks_types

module Access = struct

  module Id = Apero.Uuid

  type t = 
    { id : Id.t 
    ; path : Path.t 
    ; cache_size : int64
    ; as_string : string }

  let make ?alias path cache_size =
    let uuid = match alias with | Some(a) -> Id.make_from_alias a | None -> Id.make () in
    { id = uuid; path; cache_size; as_string = "Acc#"^(Id.to_string uuid)^"("^(Path.to_string path)^")" }

  let id a = a.id
  let alias a = Id.alias a.id
  let path a = a.path 
  let cache_size a = a.cache_size

  let to_string a =a.as_string

  let is_covering a sel = Selector.is_prefixed_by_path a.path sel


end  [@@deriving show]

