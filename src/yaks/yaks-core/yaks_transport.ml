open Yaks_access
open Apero
module Make (YEngine : Yaks_engine.SEngine.S) (MVar: Apero.MVar) = struct 
  module StorageMap = Map.Make(String)

  
  type t = { access_id: Access.Id.t; engine : YEngine.t; zenoh : Zenoh.t; smap : Zenoh.storage StorageMap.t }
    
  let make access_id engine zenoh = { access_id; engine; zenoh; smap = StorageMap.empty }

  let incoming_data_handler tx (data:IOBuf.t) (key:string) = Lwt.return_unit
  let query_handler tx  resname predicate  = Lwt.return [("", IOBuf.create 1)]

  let add_storage path tx = 
    let to_storage_path p = 
      if (String.get p (String.length p - 1)) = '/' then (Printf.sprintf ("%s**") p)
      else Printf.sprintf "%s/**" p 
    in 
    let spath = to_storage_path path in 
    match StorageMap.find_opt spath tx.smap with 
    | Some _ -> Lwt.return tx
    | _ -> 
      let storage = Zenoh.storage spath (incoming_data_handler tx) (query_handler tx) in 
      let smap' = StorageMap.add spath storage smap in 
      Lwt.return {tx with smap = smap'}
    

(*
    let add_storage path tx = 
      let to_storage_path p = 
        if (String.get p (String.length p - 1)) = '/' then (Printf.sprintf ("%s**") p)
        else Printf.sprintf "%s/**" p 
      in 
      let spath = to_storage_path path in spath
      let store = Zeno.store path 

    let remove_storage () = ()
    let register_storage engine path 
     *)

end