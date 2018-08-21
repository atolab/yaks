open Yaks_types
open Yaks_be

module Storage = struct

  module Id = struct
    include Apero.Uuid

    let of_alias = Apero.Uuid.make_from_string 
  end [@@deriving show]


  type t = 
    { id : Id.t 
    ; alias : string option
    ; path : Path.t 
    ; be : (module Backend) }

  let make ?alias path be =
    let uuid = match alias with | Some(a) -> Id.make_from_string a | None -> Id.make () in
    { id = uuid; alias; path; be }

  let id s = s.id
  let alias s = s.alias
  let path s = s.path

  let be_kind s = let module BE = (val s.be : Backend) in BE.kind

  let be s = s.be

end  [@@deriving show]

