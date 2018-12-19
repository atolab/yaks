open Yaks_access
open Yaks_types
open Apero
module Make (YEngine : Yaks_engine.SEngine.S) (MVar: Apero.MVar) = struct 
  module StorageMap = Map.Make(String)
  
  type t = { access_id: Access.Id.t; engine : YEngine.t; zenoh : Zenoh.t; smap : Zenoh.storage StorageMap.t }
    
  let make access_id engine zenoh = { access_id; engine; zenoh; smap = StorageMap.empty }

  let incoming_data_handler tx (data:IOBuf.t) (key:string) = 
    let open Yaks_fe_sock_codec in 
    match decode_value data with 
    | Ok (v, _) -> 
      (match Path.of_string_opt key with 
      | Some path -> 
        let%lwt _ = Logs_lwt.warn (fun m -> m "Inserting remote update for key %s" key) in 
        YEngine.put tx.engine tx.access_id path v  
      | _ -> 
        let%lwt _ = Logs_lwt.warn (fun m -> m "Received data for key %s which I cannot store" key) 
        in Lwt.return_unit)      
    | _ -> Lwt.return_unit

  let query_handler (*tx resname predicate*) _ _ _  = Lwt.return [("", IOBuf.create 1)]
    
  let to_zenoh_storage_path path = 
    let p = Path.to_string path in 
    if (String.get p (String.length p - 1)) = '/' then (Printf.sprintf ("%s**") p)
    else Printf.sprintf "%s/**" p 

  let add_storage (path:Path.t) (tx:t) =             
    let spath = to_zenoh_storage_path path in 
    match StorageMap.find_opt spath tx.smap with 
    | Some _ -> Lwt.return tx
    | _ -> 
      let%lwt storage = Zenoh.storage spath (incoming_data_handler tx) (query_handler tx) tx.zenoh in       
      let smap' = StorageMap.add spath storage tx.smap in 
      Lwt.return {tx with smap = smap'}
    
  let remove_storage path tx = 
    match StorageMap.find_opt (to_zenoh_storage_path path) tx.smap with 
    | Some storage ->  Zenoh.unstore storage tx.zenoh       
    | _ -> Lwt.return_unit

  
  let distribute_update path value tx =
    let res = Path.to_string path in
    let buf = IOBuf.create ~grow:8192 8192 in 
    let open Yaks_fe_sock_codec in
    match (encode_value value buf) with 
    | Ok buf -> 
      let buf' = IOBuf.flip buf in 
      Zenoh.write buf' res tx.zenoh 
    | Error e -> Lwt.fail @@ Exception e 
    
    

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