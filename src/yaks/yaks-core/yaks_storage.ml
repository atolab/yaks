open Apero
open Yaks_types
open Yaks_core_properties

module Storage = struct

  module Id = Apero.Uuid

  type t = 
    { id : Id.t
    ; path : Path.t
    ; props : properties
    ; dispose : unit -> unit Lwt.t
    ; get : Selector.t -> (Path.t * Value.t) list Lwt.t
    ; put : Path.t -> Value.t -> unit Lwt.t 
    ; put_delta : Path.t -> Value.t -> unit Lwt.t 
    ; remove : Path.t -> unit Lwt.t
    ; as_string : string
    }

  let make path props dispose get put put_delta remove =
    let alias = Properties.get Property.Storage.Key.alias props in   
    let uuid = match alias with | Some(a) -> Id.make_from_alias a | None -> Id.make () in
    { id = uuid; path; props; dispose; get; put; put_delta; remove;
      as_string = "Sto#"^(Id.to_string uuid)^"("^(Path.to_string path)^")"
    }

  let dispose s = s.dispose ()
  let id s = s.id
  let alias s = Apero.Uuid.alias s.id
  let path s = s.path
  let properties s = s.props

  let to_string s = s.as_string

  let is_covering_path s path = Path.is_prefix ~affix:s.path path
  let is_covering_selector s sel = Selector.is_prefixed_by_path s.path sel || Path.to_string s.path = Selector.path sel

  let is_conflicting s path = Path.is_prefix ~affix:path s.path || Path.is_prefix ~affix:s.path path

  let get t = t.get
  let put t = t.put
  let put_delta t = t.put_delta
  let remove t = t.remove


end  [@@deriving show]

