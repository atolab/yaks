open Apero
open Yaks_types
open Yaks_core_types
open Yaks_core_properties

module Storage = struct

  module Id = Apero.Uuid

  type t = 
    { id : Id.t
    ; selector : Selector.t
    ; props : properties
    ; dispose : unit -> unit Lwt.t
    ; get : Selector.t -> (Path.t * TimedValue.t) list Lwt.t
    ; put : Path.t -> TimedValue.t -> unit Lwt.t 
    ; put_delta : Path.t -> TimedValue.t -> unit Lwt.t 
    ; remove : Path.t -> unit Lwt.t
    ; as_string : string
    }

  let make selector props dispose get put put_delta remove =
    let alias = Properties.get Property.Storage.Key.alias props in   
    let uuid = match alias with | Some(a) -> Id.make_from_alias a | None -> Id.make () in
    { id = uuid; selector; props; dispose; get; put; put_delta; remove;
      as_string = "Sto#"^(Id.to_string uuid)^"("^(Selector.to_string selector)^")"
    }

  let dispose s = s.dispose ()
  let id s = s.id
  let alias s = Apero.Uuid.alias s.id
  let selector s = s.selector
  let properties s = s.props

  let to_string s = s.as_string

  let is_covering_path s path = Selector.is_matching_path path s.selector
  let is_covering_selector s sel = Selector.is_including_selector ~subsel:sel s.selector

  let get t = t.get
  let put t = t.put
  let update t = t.put_delta
  let remove t = t.remove


end  [@@deriving show]

