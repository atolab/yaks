open Yaks_types

module Access = struct

  module Id = struct
    include Apero.Uuid

    let of_alias = Apero.Uuid.make_from_string 
  end [@@deriving show]

  type t = 
    { id : Id.t 
    ; alias : string option
    ; path : Path.t 
    ; cache_size : int64 }

  let make ?alias path cache_size =
    let uuid = match alias with | Some(a) -> Id.make_from_string a | None -> Id.make () in
    { id = uuid; alias; path; cache_size }

  let id a = a.id
  let alias a = a.alias
  let path a = a.path 
  let cache_size a = a.cache_size
 
end  [@@deriving show]

