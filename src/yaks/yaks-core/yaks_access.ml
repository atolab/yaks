open Yaks_types

module Access = struct

  module Id = struct
    include Apero.Uuid

    let of_alias = Apero.Uuid.make_from_string 
  end [@@deriving show]

  type access_right =  R_Mode | W_Mode | RW_Mode

  type t = 
    { id : Id.t 
    ; alias : string option
    ; path : Path.t 
    ; cache_size : int64
    ; right : access_right }

  let make ?alias path cache_size right =
    let uuid = match alias with | Some(a) -> Id.make_from_string a | None -> Id.make () in
    { id = uuid; alias; path; cache_size; right }

  let id a = a.id
  let alias a = a.alias
  let path a = a.path 
  let cache_size a = a.cache_size
  let right a = a.right

  let check_access_right _ _ _ = Lwt.return_unit 

end  [@@deriving show]

