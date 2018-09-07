open Yaks_types
open Yaks_be

module Storage = struct

  module Id = Apero.Uuid

  type t = 
    { id : Id.t 
    ; path : Path.t 
    ; be : (module Backend) }

  let make ?alias path be =
    let uuid = match alias with | Some(a) -> Id.make_from_alias a | None -> Id.make () in
    { id = uuid; path; be }

  let id s = s.id
  let alias s = Id.alias s.id
  let path s = s.path

  let be_kind s = let module BE = (val s.be : Backend) in BE.kind

  let be s = s.be

end  [@@deriving show]

