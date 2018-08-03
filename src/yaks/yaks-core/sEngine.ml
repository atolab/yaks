open Yaks_types
open Yaks_property
open Yaks_be 
open Apero.LwtM.InfixM


module SEngine = struct

  module type S = sig 
    type t 
      
    val make : unit -> t 
    val create_access : t  -> Path.t -> int64 ->  AccessId.t Lwt.t
    val create_access_with_id : t -> Path.t -> int64 ->  AccessId.t -> unit Lwt.t
    val get_access : t -> AccessId.t -> AccessId.t option Lwt.t
    val dispose_access : t -> AccessId.t -> unit Lwt.t

    val create_storage : t -> Path.t -> Property.t list -> StorageId.t Lwt.t 
    val create_storage_with_id : t -> Path.t -> Property.t list -> StorageId.t -> unit Lwt.t 
    val get_storage : t -> StorageId.t -> StorageId.t option Lwt.t
    val dispose_storage : t -> StorageId.t -> unit Lwt.t

    val create_subscriber : t -> Path.t -> Selector.t -> bool -> SubscriberId.t Lwt.t  

    val get : t -> AccessId.t -> Selector.t -> (string * Value.t) list  Lwt.t

    val put : t -> AccessId.t -> Selector.t -> Value.t -> unit Lwt.t
    val put_delta : t -> AccessId.t -> Selector.t -> Value.t -> unit Lwt.t

    val remove : t -> AccessId.t -> Selector.t -> unit Lwt.t

    val add_backend_factory : t -> string -> (module BackendFactory) -> unit Lwt.t
  end

  module Make (MVar: Apero.MVar) = struct
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
    

    type state = 
      { bes : backend_info BackendMap.t 
      ; befs : (module Yaks_be.BackendFactory) BackendFactoryMap.t 
      ; accs : access_info AccessMap.t } 
      
    type t = state MVar.t
    
    (* 
    let create_access engine path cache_size = (engine, Lwt.return @@ AccessId.next_id ())
    let create_storage engine path properties = (engine, Lwt.return   @@ StorageId.next_id ()  )
    let create_subscriber engine path selector push = (engine, Lwt.return @@  SubscriberId.next_id ()) 
    let add_back_end engine be_name be_module = (engine, Lwt.return_unit)
    *)

    let make () = 
      let _ = Logs_lwt.debug (fun m -> m "Creating Engine\n") in 
      MVar.create 
      { bes = BackendMap.empty 
      ; befs = BackendFactoryMap.empty 
      ; accs = AccessMap.empty }

    (* @TODO: Implement read/write check  *)
    let check_write_access _ _ _ (*self access_info path*) = Lwt.return_unit
    let check_read_access (* self access_info selector *) _ _ (_ : Selector.t) = Lwt.return_unit  

    (* Checks if the access can read data addressed by this selector. It 
      returns a Lwt.fail with the proper exception set if the rights are not 
      sufficient *)
    type access_check = state -> access_info -> Selector.t -> unit Lwt.t

    let get_matching_bes (self:state) (access_id:AccessId.t) (selector: Selector.t) (access_controller : access_check)  = 
       match AccessMap.find_opt access_id  self.accs with 
        | Some access -> 
          Lwt.try_bind 
            (fun () -> access_controller self access selector)
            (fun () -> 
              Lwt.return @@
              BackendMap.filter 
                (fun _ (info:backend_info) -> Path.is_prefix info.path (Selector.path selector)) 
                self.bes)
            (fun e -> Lwt.fail e)
        | None -> Lwt.fail (YException (`UnkownAccessId (`Msg (AccessId.to_string access_id))))

    let create_access_with_id engine path cache_size access_id = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.create_access path: %s " (Path.to_string path)) in    
      let info = { uid = access_id ; path ; cache_size } in 
      MVar.guarded engine
        (fun self -> MVar.return () {self with accs = (AccessMap.add access_id info self.accs)})

    let create_access engine path cache_size = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.create_access path: %s " (Path.to_string path)) in
      let uid = AccessId.next_id () in 
      create_access_with_id engine path cache_size uid >|= fun () -> uid    
      
    let get_access engine access_id = 
      MVar.read engine      
      >|= fun self -> Apero.Option.bind (AccessMap.find_opt access_id self.accs) (fun _ -> Some access_id)

    let dispose_access engine access_id = 
      MVar.guarded engine @@ 
      fun self -> 
        let self' = {self with accs =  AccessMap.remove access_id self.accs } in 
        MVar.return () self'

    let create_storage_with_id engine path properties storage_id =                 
      match get_property yaks_backend properties with 
      | Some (_,v) -> 
        let _ = Logs_lwt.debug (fun m -> m "SEngine:  create %s storage\n" v) in      
        MVar.guarded engine 
        @@ fun (self:state) -> 
          (match BackendFactoryMap.find_opt v self.befs with
          | Some bef -> 
            let module BEF = (val bef : BackendFactory) in 
            let bem =  BEF.make path properties in
            let be_info = 
              { kind = BEF.kind
              ; uid = storage_id
              ; path
              ; be = bem } in 
           MVar.return () {self with bes = (BackendMap.add storage_id be_info self.bes)}          
          | None ->           
            MVar.return_lwt (Lwt.fail @@ YException (`UnavailableStorageFactory (`Msg v))) self)
      | None -> Lwt.fail @@ YException `UnknownStorageKind

    let create_storage engine path properties =     
      let%lwt _ = Logs_lwt.debug (fun m -> m "Engine:  create %s storage\n" (Path.to_string path)) in      
      let id = StorageId.next_id () in 
      create_storage_with_id engine path properties id >|= fun () -> id
      
    let get_storage engine storage_id = 
      MVar.read engine >>=
      (fun self -> Lwt.return
        (Apero.Option.bind (BackendMap.find_opt storage_id self.bes) (fun _ -> Some storage_id)))

    let dispose_storage engine storage_id = 
      MVar.guarded engine 
      @@ fun self -> 
        let self' = {self with bes =  BackendMap.remove storage_id self.bes } in 
        MVar.return () self'

    let create_subscriber _ _ _ _ = Lwt.return @@  SubscriberId.next_id ()
    
   
    let be_get be selector = 
      let module BE = (val be : Backend) in 
      BE.get selector

    let get engine access_id selector = 
    let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: put") in      
      MVar.read engine 
      >>= fun self ->       
        get_matching_bes self access_id selector check_read_access
        >>= fun mbes -> 
         let (m_be, o_be) = mbes          
          |> BackendMap.partition (fun _ info -> info.kind = Yaks_be.Memory)  in 
          (* Try to always resolve Get out of memory back-end if available *)
          (match BackendMap.find_first_opt (fun _ -> true) m_be with 
          | Some (_, info) -> be_get info.be selector 
          | None -> 
            (match BackendMap.find_first_opt (fun _ -> true) o_be with 
            | Some (_, info) -> be_get info.be selector
            | None -> Lwt.return []))
          
            

    let be_put be selector = 
      let module BE = (val be: Backend) in 
      BE.put selector 

    let put (engine: t) access_id (selector:Selector.t) (value:Value.t) =     
      let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: put") in      
      MVar.read engine 
      >>= fun self ->       
        get_matching_bes self access_id selector check_write_access
        >>= fun mbes -> 
          mbes |> BackendMap.iter (fun _ (info:backend_info) -> let _ = be_put info.be selector value in ()) 
          ; Lwt.return_unit
        

    let be_put_delta be selector =   
      let module BE = (val be: Backend) in 
      BE.put_delta selector
    
    let put_delta engine access_id selector value = 
    let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: put_delta") in      
      MVar.read engine 
      >>= fun self ->       
        get_matching_bes self access_id selector check_write_access
        >>= fun mbes -> 
          mbes |> BackendMap.iter (fun _ (info:backend_info) -> let _ = be_put_delta info.be selector value in ()) 
          ; Lwt.return_unit

    let be_remove be = 
      let module BE = (val be: Backend) in BE.remove

    let remove engine access_id selector = 
      let%lwt self = MVar.read engine in       
      get_matching_bes self access_id selector check_write_access
      >>= fun bes -> 
        Lwt.return @@ BackendMap.iter (fun _ bei -> Lwt.ignore_result @@ be_remove bei.be selector) bes                 

    let add_backend_factory engine name factory =  
      Logs_lwt.debug (fun m -> m "add_backend_factory : %s" name) >>
      MVar.guarded engine 
      @@ fun self ->         
        MVar.return () { self with befs = BackendFactoryMap.add name factory self.befs }
  end

end


