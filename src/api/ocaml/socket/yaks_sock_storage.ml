open Lwt.Infix
open Yaks_sock_types

module MVar = Apero.MVar_lwt
module Storage = struct


  type state = {
    path : Yaks_types.Path.t
  ; properties : String.t Yaks_core.Property.Map.t
  ; sid : StorageId.t
  ; driver : Yaks_sock_driver.t
  }

  type t = state MVar.t

  let create properties path id driver = 
    MVar.create {sid=id; path; properties; driver}

  let get_id storage = 
    MVar.read storage >>= fun s -> Lwt.return s.sid

end