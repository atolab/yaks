open Yaks_types

module Access = struct

  module Id = Apero.Uuid

  type t = 
    { id : Id.t 
    ; path : Path.t 
    ; cache_size : int64 }

  let make ?alias path cache_size =
    let uuid = match alias with | Some(a) -> Id.make_from_alias a | None -> Id.make () in
    { id = uuid; path; cache_size }

  let id a = a.id
  let alias a = Id.alias a.id
  let path a = a.path 
  let cache_size a = a.cache_size

end  [@@deriving show]

