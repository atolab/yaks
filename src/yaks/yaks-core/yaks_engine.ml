open Yaks_types
open Yaks_property
open Yaks_access
open Yaks_be 
open Apero.LwtM.InfixM
open Yaks_user
open Yaks_group

module SEngine = struct

  module type S = sig 
    type t 
      
    val make : unit -> t 
    val create_access : t  -> Path.t -> int64 -> User.Id.t ->  Access.Id.t Lwt.t
    val create_access_with_id : t -> Path.t -> int64 -> User.Id.t  -> Access.Id.t -> unit Lwt.t
    val get_access : t -> Access.Id.t -> Access.Id.t option Lwt.t
    val dispose_access : t -> Access.Id.t -> unit Lwt.t

    val create_storage : t -> Path.t -> Property.t list -> StorageId.t Lwt.t 
    val create_storage_with_id : t -> Path.t -> Property.t list -> StorageId.t -> unit Lwt.t 
    val get_storage : t -> StorageId.t -> StorageId.t option Lwt.t
    val dispose_storage : t -> StorageId.t -> unit Lwt.t

    val create_group : t -> string -> Selector.t list -> Selector.t list -> Selector.t list -> Group.group_level -> Group.Id.t Lwt.t
    val create_group_with_id : t -> string -> Selector.t list -> Selector.t list -> Selector.t list -> Group.group_level -> Group.Id.t -> unit Lwt.t

    val create_user : t -> string -> string -> Group.Id.t -> User.Id.t Lwt.t
    val create_user_with_id : t -> string -> string -> Group.Id.t -> User.Id.t -> unit Lwt.t
    val authenticate_user : t -> string -> string -> User.Id.t Lwt.t 

    val create_subscriber : t -> Path.t -> Selector.t -> bool -> SubscriberId.t Lwt.t  

    val get : t -> Access.Id.t -> Selector.t -> (string * Value.t) list  Lwt.t

    val put : t -> Access.Id.t -> Selector.t -> Value.t -> unit Lwt.t
    val put_delta : t -> Access.Id.t -> Selector.t -> Value.t -> unit Lwt.t

    val remove : t -> Access.Id.t -> Selector.t -> unit Lwt.t

    val add_backend_factory : t -> string -> (module BackendFactory) -> unit Lwt.t
  end

  module Make (MVar: Apero.MVar) = struct
    type backend_info = 
      { kind : backend_kind
      ; uid : StorageId.t
      ; path : Path.t
      ; be : (module Backend) }

    type access_info = 
      { uid : Access.Id.t 
      ; path : Path.t 
      ; cache_size : int64
      ; right : Access.access_right } 
      (* at some point we may also keep a list of  backend_info matching this access  *)
    
    module BackendFactoryMap = Map.Make (String)  
    module BackendMap  = Map.Make (StorageId)
    module AccessMap = Map.Make (Access.Id)
    
    module GroupMap = Map.Make(Group.Id)
    module UserMap = Map.Make(User.Id) 


    type state = 
      { bes : backend_info BackendMap.t 
      ; befs : (module Yaks_be.BackendFactory) BackendFactoryMap.t 
      ; accs : access_info AccessMap.t
      ; groups : Group.t GroupMap.t
      ; users : User.t UserMap.t
     } 
      
    type t = state MVar.t
    
    (* 
    let create_access engine path cache_size = (engine, Lwt.return @@ Access.Id.next_id ())
    let create_storage engine path properties = (engine, Lwt.return   @@ StorageId.next_id ()  )
    let create_subscriber engine path selector push = (engine, Lwt.return @@  SubscriberId.next_id ()) 
    let add_back_end engine be_name be_module = (engine, Lwt.return_unit)
    *)

    let make () = 
      let _ = Logs_lwt.debug (fun m -> m "Creating Engine\n") in 
      MVar.create 
      { bes = BackendMap.empty 
      ; befs = BackendFactoryMap.empty 
      ; accs = AccessMap.empty
      ; groups = GroupMap.empty
      ; users = UserMap.empty
      }

    (* @TODO: This check should also verify that the user is in the right groups 
      Nasty users can use access coming from an authorized user to get access to data
      that they may not have access rights.
      So the check should be at access level and user level
      GB: We can store the user id in the access, in this way an user is coupled to one or more access, but an access can be used only by
      the user that created that access
     *)
    let check_write_access _ access_info path (*self access_info path*) = 
      if Selector.match_path path access_info.path then
        match access_info.right with
        | RW_Mode ->  
          let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.check_write_access access granted for: %s by permissions" (Selector.to_string path)) in
          Lwt.return_unit
        | W_Mode -> 
          let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.check_write_access access granted for: %s by permissions" (Selector.to_string path)) in
          Lwt.return_unit
        | R_Mode -> 
          let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.check_write_access access denied for: %s by permissions" (Selector.to_string path)) in
          Lwt.fail @@ YException (`UnauthorizedAccess (`Msg (Printf.sprintf "Cannot write to %s" (Selector.to_string path))))
      else
        let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.check_write_access access denied for: %s by matching" (Selector.to_string path)) in
        Lwt.fail @@ YException (`UnauthorizedAccess (`Msg (Printf.sprintf "Cannot write to %s" (Selector.to_string path))))
      
    let check_read_access (* self access_info selector *) _ access_info selector =
    if Selector.match_path selector access_info.path then
      match access_info.right with
      | R_Mode -> 
        let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.check_read_access access granted for: %s by permissions" (Selector.to_string selector)) in
        Lwt.return_unit
      | RW_Mode -> 
        let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.check_read_access access granted for: %s by permissions" (Selector.to_string selector)) in
        Lwt.return_unit
      | W_Mode -> 
        let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.check_read_access access denied for: %s by permissions" (Selector.to_string selector)) in
        Lwt.fail @@ YException (`UnauthorizedAccess (`Msg (Printf.sprintf "Cannot read from to %s" (Selector.to_string selector))))
    else
      let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.check_read_access access denied for: %s by matching" (Selector.to_string selector)) in
      Lwt.fail @@ YException (`UnauthorizedAccess (`Msg (Printf.sprintf "Cannot read from to %s" (Selector.to_string selector))))
     

    (* Checks if the access can read data addressed by this selector. It 
      returns a Lwt.fail with the proper exception set if the rights are not 
      sufficient *)
    type access_check = state -> access_info -> Selector.t -> unit Lwt.t

    let get_matching_bes (self:state) (access_id : Access.Id.t) (selector: Selector.t) (access_controller : access_check) =
       match AccessMap.find_opt access_id self.accs with
        | Some access ->
          Lwt.try_bind
            (fun () -> access_controller self access selector)
            (fun () ->
              Lwt.return @@
              BackendMap.filter
                (fun _ (info:backend_info) -> Path.is_prefix info.path (Selector.path selector))
                self.bes)
            (fun e -> Lwt.fail e)
        | None ->
          let ei : error_info = `Msg (Access.Id.to_string access_id) in
          let err : yerror = `UnknownAccess ei in
          Lwt.fail @@ YException err

    let create_group_with_id engine name rw_paths r_paths w_paths level group_id = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.create group id: %s " (Group.Id.to_string group_id)) in
      let g = Group.{id=group_id; name; rw_paths; r_paths; w_paths; group_level=level} in
      MVar.guarded engine 
        (fun self ->  MVar.return () {self with groups = (GroupMap.add group_id g self.groups)})

    let create_group engine name rw_paths r_paths w_paths level = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.create group name: %s " name) in
      let uid = Group.Id.next_id () in
      create_group_with_id engine name rw_paths r_paths w_paths level uid >|= fun () -> uid
    
    let create_user_with_id engine name password group user_id =
      let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.create user id: %s " (User.Id.to_string user_id)) in
      let u = User.{id=user_id; name; password; group } in
      MVar.guarded engine 
        (fun self ->  MVar.return () {self with users = (UserMap.add user_id u self.users)})

    let create_user engine name password group = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.create user name: %s " name) in
      let uid = User.Id.next_id () in
      create_user_with_id engine name password group uid >|= fun () -> uid

    let authenticate_user engine name password =
      MVar.read engine >>= (fun e ->
      let b = UserMap.bindings e.users in
      let open User in
      let k,_ = List.find (fun (_,v) -> (v.name=name && v.password = password) ) b in
      Lwt.return k)


    let create_access_with_id engine path cache_size user_id access_id = (* unsed is userid *)
      let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.create_access path: %s " (Path.to_string path)) in
      (* Should get the user groups and depend on the group assign the rights for the access *)
      (* The map between security token and userid should be managed at access level *)
      (* An user is part of only one group *)
      MVar.guarded engine 
        @@ fun (self:state) -> 
          (match UserMap.find_opt user_id self.users with
          | Some u -> 
                let g = GroupMap.find u.group self.groups in
                let create info =
                  MVar.return () {self with accs = (AccessMap.add access_id info self.accs)}
                in
                (match (List.exists (fun ie -> Selector.match_path ie path) g.rw_paths),(List.exists (fun ie -> Selector.match_path ie path) g.r_paths),(List.exists (fun ie -> Selector.match_path ie path) g.w_paths) with
                | (true,_,_) -> create { uid = access_id ; path ; cache_size; right = RW_Mode }
                | (false,true,false) ->create { uid = access_id ; path ; cache_size; right = R_Mode}
                | (false,false,true) -> create { uid = access_id ; path ; cache_size; right = W_Mode}
                | (false,false,false) -> 
                  let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.create_access cannot create access for path %s user has no rights" (Path.to_string path)) in
                  let v = Printf.sprintf "No rights for path %s" @@ Path.to_string path in
                  MVar.return_lwt (Lwt.fail @@ YException (`Forbidden (`Msg v))) self
                | _ -> 
                  let v = Printf.sprintf "Group is ill formed no rights for path %s" @@ Path.to_string path in
                  MVar.return_lwt (Lwt.fail @@ YException (`Forbidden (`Msg v))) self
                )
          | None -> 
            let v = Printf.sprintf "User %s Unknown" @@ User.Id.to_string user_id in
            MVar.return_lwt (Lwt.fail @@ YException (`Forbidden (`Msg v))) self)
        
    

    let create_access engine path cache_size userid =
      let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.create_access path: %s " (Path.to_string path)) in
      let uid = Access.Id.next_id () in
      create_access_with_id engine path cache_size userid uid >|= fun () -> uid
      
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
    let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: get") in
      MVar.read engine 
      >>= fun self ->
        get_matching_bes self access_id selector check_read_access
        >>= fun mbes -> 
         let (m_be, o_be) = mbes
          |> BackendMap.partition (fun _ info -> info.kind = Yaks_be.Memory) in
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


