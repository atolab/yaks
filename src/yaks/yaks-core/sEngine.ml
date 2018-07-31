open Yaks_be 
open Yaks_const
open Apero.LwtM.InfixM
open Yaks_types

module SEngine = struct 

  type backend_info = 
    { kind : backend_kind
    ; uid : StorageId.t
    ; path : Path.t
    ; be : (module Backend) }

  type access_info = 
    { uid : AccessId.t 
    ; path : Path.t 
    ; cache_size : int64 } 
    (* at some point we may also keep a list of  backend_info matching this access  *)
  
  module BackendFactoryMap = Map.Make (String)  
  module BackendMap  = Map.Make (StorageId)
  module AccessMap = Map.Make (AccessId)
  
  type t = 
    { bes : backend_info BackendMap.t 
    ; befs : (module Yaks_be.BackendFactory) BackendFactoryMap.t 
    ; accs : access_info AccessMap.t }
    
  
  (* 
  let create_access engine path cache_size = (engine, Lwt.return @@ AccessId.next_id ())
  let create_storage engine path properties = (engine, Lwt.return   @@ StorageId.next_id ()  )
  let create_subscriber engine path selector push = (engine, Lwt.return @@  SubscriberId.next_id ()) 
  let add_back_end engine be_name be_module = (engine, Lwt.return_unit)
  *)

  let make () = Lwt_mvar.create 
    { bes = BackendMap.empty 
    ; befs = BackendFactoryMap.empty 
    ; accs = AccessMap.empty }

  let create_access_with_id engine path cache_size access_id = 
    let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.create_access path: %s " (Path.to_string path)) in    
    let info = { uid = access_id ; path ; cache_size } in 
    let%lwt self = Lwt_mvar.take engine in
    Lwt_mvar.put engine { self with accs = (AccessMap.add access_id info self.accs)}

  let create_access engine path cache_size = 
    let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.create_access path: %s " (Path.to_string path)) in
    let uid = AccessId.next_id () in 
    create_access_with_id engine path cache_size uid >|= fun () -> uid    
    
  let get_access engine access_id = 
    Apero.LwtM.read_mvar engine 
    >|= fun self -> Apero.Option.bind (AccessMap.find_opt access_id self.accs) (fun _ -> Some access_id)

  let create_storage_with_id engine path properties storage_id =           
    match List.find_opt (fun p -> Property.key p = yaks_backend_kind) properties with 
    | Some (_,v) -> 
      let _ = Logs_lwt.debug (fun m -> m "SEngine:  create %s storage\n" v) in      
      let%lwt self = Lwt_mvar.take engine in       
      (match BackendFactoryMap.find_opt v  self.befs with
      | Some bef -> 
        let module BEF = (val bef : BackendFactory) in 
        let bem =  BEF.make path properties in
        let be_info = 
          { kind = BEF.kind
          ; uid = storage_id
          ; path
          ; be = bem } in 
        Lwt_mvar.put engine {self with bes = (BackendMap.add storage_id be_info self.bes)} 
        
      | None -> 
        Lwt_mvar.put  engine self 
        >> Lwt.fail @@ YException (`UnavailableStorageFactory (`Msg v)) )
      
    | None -> Lwt.fail @@ YException `UnknownStorageKind

  let create_storage engine path properties =     
    let id = StorageId.next_id () in 
    create_storage_with_id engine path properties id >|= fun () -> id
    
  let get_storage engine storage_id = 
    Apero.LwtM.read_mvar engine 
    >|= fun self -> Apero.Option.bind (BackendMap.find_opt  storage_id self.bes) (fun _ -> Some storage_id)

  let create_subscriber _ _ _ _ = Lwt.return @@  SubscriberId.next_id ()
  
  (* @TODO: Implement read/write check  *)
  let check_write_access _ _ _ (*self access path*) = Lwt.return_unit
  let check_read_access (* self access selector *) _ _ (_ : Selector.t) = Lwt.return_unit  
  (* Checks if the access can read data addressed by this selector. It 
     returns a Lwt.fail with the proper exception set if the rights are not 
     sufficient *)

  let get_from_be be selector = 
    let module BE = (val be : Backend) in 
    BE.get selector

  let get engine access_id selector = 
    let%lwt self = Apero.LwtM.read_mvar engine in 
    check_read_access self access_id selector 
    >>= fun () ->       
      match (AccessMap.find_opt access_id self.accs) with 
      | Some access_info ->         
        let (m_be, o_be) = self.bes
        |> BackendMap.filter (fun _ (info: backend_info) -> Path.is_prefix info.path access_info.path) 
        |> BackendMap.partition (fun _ info -> info.kind = Yaks_be.Memory)  in 
        (* Try to always resolve Get out of memory back-end if available *)
        (match BackendMap.find_first_opt (fun _ -> true) m_be with 
        | Some (_, info) -> get_from_be info.be selector 
        | None -> 
          (match BackendMap.find_first_opt (fun _ -> true) o_be with 
          | Some (_, info) -> get_from_be info.be selector
          | None -> Lwt.return []))
    
      | None -> Lwt.fail (YException (`UnkownAccessId (`Msg (AccessId.to_string access_id))))        


  let put_onto_be be kv = 
    let module BE = (val be: Backend) in 
    BE.put kv

  let put (engine: t Lwt_mvar.t) access_id (key:string) (value:Value.t) =     
    let%lwt self = Apero.LwtM.read_mvar engine in     
    match AccessMap.find_opt access_id  self.accs with 
    | Some access -> 
      Lwt.try_bind 
        (fun () -> check_write_access self access key)
        (fun () -> 
          let _ = self.bes 
          |> BackendMap.filter (fun _ (info:backend_info) -> Path.is_prefix info.path (Path.of_string key))
          |> BackendMap.iter (fun _ (info:backend_info) -> let _ = put_onto_be info.be key value in ()) in
          Lwt.return_unit                              
        )
        (fun e -> Lwt.fail e)
    | None -> Lwt.fail (YException (`UnkownAccessId (`Msg (AccessId.to_string access_id))))

  let add_backend_factory engine name factory =  
    Logs_lwt.debug (fun m -> m "add_backend_factory : %s" name) >>
    let%lwt self = Lwt_mvar.take engine in     
    Lwt_mvar.put engine { self with befs = BackendFactoryMap.add name factory self.befs }
end
