open Yaks_types
open Yaks_property
open Yaks_access
open Yaks_storage
open Yaks_be 
open Apero.LwtM.InfixM
open Yaks_user
open Yaks_group

module SEngine = struct

  module type S = sig 
    type t 

    val make : unit -> t 
    val create_access : t  -> ?alias:string -> Path.t -> int64 -> User.Id.t ->  Access.t Lwt.t
    val get_access : t -> Access.Id.t -> Access.t option Lwt.t
    val dispose_access : t -> Access.Id.t -> unit Lwt.t

    val create_storage : t -> ?alias:string -> Path.t -> Property.t list -> Storage.t Lwt.t 
    val get_storage : t -> Storage.Id.t -> Storage.t option Lwt.t
    val dispose_storage : t -> Storage.Id.t -> unit Lwt.t

    (* @AC: I am not convinced that group  user management functionalities belong to YAKS. 
            This should be provided by another service which YAKS relies upon. 
            The specifics of how the athentication happens, etc. should be transparent for YAKS.
            The only thing that YAKS should do is to check for access with this access management 
            module and act accordingly.                     
    *)
    val create_group : t -> string -> Selector.t list -> Selector.t list -> Selector.t list -> Group.group_level -> Group.Id.t Lwt.t
    val create_group_with_id : t -> string -> Selector.t list -> Selector.t list -> Selector.t list -> Group.group_level -> Group.Id.t -> unit Lwt.t
    val dispose_group : t -> Group.Id.t -> unit Lwt.t 

    val create_user : t -> string -> string -> Group.Id.t -> User.Id.t Lwt.t
    val create_user_with_id : t -> string -> string -> Group.Id.t -> User.Id.t -> unit Lwt.t
    val authenticate_user : t -> string -> string -> User.Id.t Lwt.t
    val dispose_user : t -> User.Id.t -> unit Lwt.t 

    val create_subscriber : t -> Path.t -> Selector.t -> bool -> SubscriberId.t Lwt.t  

    val get : t -> Access.Id.t -> Selector.t -> (string * Value.t) list  Lwt.t

    val put : t -> Access.Id.t -> Selector.t -> Value.t -> unit Lwt.t
    val put_delta : t -> Access.Id.t -> Selector.t -> Value.t -> unit Lwt.t

    val remove : t -> Access.Id.t -> Selector.t -> unit Lwt.t

    val add_backend_factory : t -> string -> (module BackendFactory) -> unit Lwt.t

    val add_security : t -> (module Yaks_sec.Security) -> unit Lwt.t
    val is_secure : t -> bool Lwt.t

  end

  module Make (MVar: Apero.MVar) = struct

    module BackendFactoryMap = Map.Make (String)  
    module StorageMap  = Map.Make (Storage.Id)
    module AccessMap = Map.Make (Access.Id)

    module GroupMap = Map.Make(Group.Id)
    module UserMap = Map.Make(User.Id) 


    type state = 
      { befs : (module Yaks_be.BackendFactory) BackendFactoryMap.t 
      ; stores : Storage.t StorageMap.t
      ; accs : Access.t AccessMap.t
      ; sec : (module Yaks_sec.Security) option
      (* ; groups : Group.t GroupMap.t
         ; users : User.t UserMap.t *)
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
        { befs = BackendFactoryMap.empty 
        ; stores = StorageMap.empty
        ; accs = AccessMap.empty
        ; sec = None
          (* ; groups = GroupMap.empty
             ; users = UserMap.empty  *)
        }



    (* Checks if the access can read data addressed by this selector. It 
       returns a Lwt.fail with the proper exception set if the rights are not 
       sufficient *)
    type access_check = Access.t -> Selector.t -> unit Lwt.t

    let get_matching_stores (self:state) (access_id : Access.Id.t) (selector: Selector.t) (access_controller : access_check) =
      match AccessMap.find_opt access_id self.accs with
      | Some access ->
        Lwt.try_bind
          (fun () -> access_controller access selector)
          (fun () ->
             Lwt.return @@
             StorageMap.filter
               (fun _ (s:Storage.t) -> Path.is_prefix (Storage.path s) (Selector.path selector))
               self.stores)
          (fun e -> Lwt.fail e)
      | None ->
        let ei : error_info = `Msg (Access.Id.to_string access_id) in
        let err : yerror = `UnknownAccess ei in
        Lwt.fail @@ YException err

    let create_group_with_id engine name rw_paths r_paths w_paths level group_id = 
      (* 
      Create a group with the parameters, return unit
     *)
      MVar.read engine >>= (fun state -> 
          let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.create_group  id: %s " (Group.Id.to_string group_id)) in
          match state.sec with
          | Some sec -> 
            let module Sec = (val sec : Yaks_sec.Security) in
            Sec.create_group_with_id name rw_paths r_paths w_paths level group_id
          | None -> Lwt.return_unit)


    let create_group engine name rw_paths r_paths w_paths level = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.create_group name: %s " name) in
      let uid = Group.Id.make () in
      create_group_with_id engine name rw_paths r_paths w_paths level uid >|= fun () -> uid

    let dispose_group engine group_id =
      MVar.read engine >>= (fun state -> 
          match state.sec with
          | Some sec -> 
            let module Sec = (val sec : Yaks_sec.Security) in
            Sec.dispose_group group_id
          | None -> Lwt.return_unit)


    let create_user_with_id engine name password group user_id =
      MVar.read engine >>= (fun state -> 
          match state.sec with
          | Some sec -> 
            let module Sec = (val sec : Yaks_sec.Security) in
            let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.create_user_with_id id: %s " (User.Id.to_string user_id)) in
            Sec.create_user_with_id name password group user_id
          | None -> Lwt.return_unit)


    let create_user engine name password group = 

      let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.create_user name: %s " name) in
      let uid = User.Id.make () in
      create_user_with_id engine name password group uid >|= fun () -> uid

    let authenticate_user engine name password =
      MVar.read engine >>= (fun state -> 
          match state.sec with
          | Some sec -> 
            let module Sec = (val sec : Yaks_sec.Security) in
            let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.authenticate_user name: %s " name) in
            Sec.authenticate_user name password
          | None -> Lwt.return @@ User.Id.make ())



    let dispose_user engine user_id = 
      MVar.read engine >>= (fun state -> 
          match state.sec with
          | Some sec -> 
            let module Sec = (val sec : Yaks_sec.Security) in
            Sec.dispose_user user_id
          | None -> Lwt.return_unit)


    let create_access engine ?alias path cache_size user_id = (* unsed is userid *)

      let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.create_access path: %s  alias: %s" (Path.to_string path) (Apero.Option.get_or_default alias "-")) in
      (* Should get the user groups and depend on the group assign the rights for the access *)
      (* The map between security token and userid should be managed at access level *)
      (* An user is part of only one group *)
      MVar.guarded engine 
      @@ fun (self:state) -> 
      (match self.sec with
       | Some sec -> 
         let module Sec = (val sec : Yaks_sec.Security) in
         Sec.get_user user_id >>= 
         (fun pu -> 
            (match pu with
             | Some u -> 
               Sec.get_group u.group >>=
               (fun pg -> 
                  (match pg with
                   | Some g -> 
                     let create access =
                       MVar.return (access) {self with accs = (AccessMap.add (Access.id access) access self.accs)}
                     in
                     (match Sec.get_access_creation_rights g path with
                      | Ok r -> create @@ Access.make ?alias path cache_size r
                      | Error e -> 
                        MVar.return_lwt (Lwt.fail @@ YException e) self)
                   | None -> 
                     let v = Printf.sprintf "User %s not allowed" @@ User.Id.to_string user_id in
                     MVar.return_lwt (Lwt.fail @@ YException (`Forbidden (`Msg v))) self)
               )

             | None -> 
               let v = Printf.sprintf "User %s Unknown" @@ User.Id.to_string user_id in
               MVar.return_lwt (Lwt.fail @@ YException (`Forbidden (`Msg v))) self))
       | None -> 
         let create access =
           MVar.return (access) {self with accs = (AccessMap.add (Access.id access) access self.accs)}
         in
         create @@ Access.make ?alias path cache_size Access.RW_Mode
      )


    let get_access engine access_id =
      MVar.read engine
      >|= fun self -> AccessMap.find_opt access_id self.accs

    let dispose_access engine access_id =
      MVar.guarded engine @@
      fun self ->
      let self' = {self with accs =  AccessMap.remove access_id self.accs } in
      MVar.return () self'

    let create_storage_with_kind engine ?alias path kind = 
      let _ = Logs_lwt.debug (fun m -> m "SEngine:  create %s storage\n" kind) in
      MVar.guarded engine
      @@ fun (self:state) ->
      (match BackendFactoryMap.find_opt kind self.befs with
       | Some bef ->
         let module BEF = (val bef : BackendFactory) in
         let bem =  BEF.make [] in
         let s = Storage.make ?alias path bem in
         MVar.return (s) {self with stores = (StorageMap.add (Storage.id s) s self.stores)}
       | None ->
         MVar.return_lwt (Lwt.fail @@ YException (`UnavailableStorageFactory (`Msg kind))) self)

    let create_storage engine ?alias path properties =
      match get_property yaks_backend properties with
      | Some (_,v) ->
        create_storage_with_kind engine ?alias path v
      | None -> 
        let _ = Logs_lwt.debug (fun m -> m "SEngine:  Creating with default Memory Backend\n") in
        create_storage_with_kind engine ?alias path yaks_backend_memory
    (* Lwt.fail @@ YException `UnknownStorageKind *)

    let get_storage engine storage_id =
      MVar.read engine >|= fun self -> StorageMap.find_opt storage_id self.stores

    let dispose_storage engine storage_id =
      MVar.guarded engine
      @@ fun self ->
      let self' = {self with stores =  StorageMap.remove storage_id self.stores } in
      MVar.return () self'

    let create_subscriber _ _ _ _ = Lwt.return @@  SubscriberId.next_id ()


    let be_get be selector =
      let module BE = (val be : Backend) in
      BE.get selector

    let get engine access_id selector =
      let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: get") in
      MVar.read engine 
      >>= fun self ->
      let check = 
        match self.sec with
        | Some sec -> 
          let module Sec = (val sec : Yaks_sec.Security) in
          Sec.check_read_access
        | None -> 
          fun _ _  -> Lwt.return_unit
      in
      get_matching_stores self access_id selector check
      >>= fun stores -> 
      let (m_be, o_be) =
        stores |> StorageMap.partition (fun _ s -> Storage.be_kind s = Yaks_be.Memory) in
      (* Try to always resolve Get out of memory back-end if available *)
      (match StorageMap.find_first_opt (fun _ -> true) m_be with 
       | Some (_, s) -> be_get (Storage.be s) selector 
       | None ->
         (match StorageMap.find_first_opt (fun _ -> true) o_be with 
          | Some (_, s) -> be_get (Storage.be s) selector
          | None -> Lwt.return []))



    let be_put be selector = 
      let module BE = (val be: Backend) in 
      BE.put selector 

    let put (engine: t) access_id (selector:Selector.t) (value:Value.t) =
      let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: put") in
      MVar.read engine 
      >>= fun self ->
      let check = 
        match self.sec with
        | Some sec -> 
          let module Sec = (val sec : Yaks_sec.Security) in
          Sec.check_read_access
        | None -> 
          fun _ _  -> Lwt.return_unit
      in
      get_matching_stores self access_id selector check
      >>= fun stores ->
      stores |> StorageMap.iter (fun _ (s:Storage.t) -> let _ = be_put (Storage.be s) selector value in ()) 
    ; Lwt.return_unit


    let be_put_delta be selector =
      let module BE = (val be: Backend) in 
      BE.put_delta selector

    let put_delta engine access_id selector value = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: put_delta") in
      MVar.read engine 
      >>= fun self ->
      let check = 
        match self.sec with
        | Some sec -> 
          let module Sec = (val sec : Yaks_sec.Security) in
          Sec.check_read_access
        | None -> 
          fun _ _  -> Lwt.return_unit
      in
      get_matching_stores self access_id selector check
      >>= fun stores ->
      stores |> StorageMap.iter (fun _ s -> let _ = be_put_delta (Storage.be s) selector value in ()) 
    ; Lwt.return_unit

    let be_remove be = 
      let module BE = (val be: Backend) in BE.remove

    let remove engine access_id selector = 
      let%lwt self = MVar.read engine in
      let check = 
        match self.sec with
        | Some sec -> 
          let module Sec = (val sec : Yaks_sec.Security) in
          Sec.check_read_access
        | None -> 
          fun _ _  -> Lwt.return_unit
      in
      get_matching_stores self access_id selector check
      >>= fun stores -> 
      Lwt.return @@ StorageMap.iter (fun _ s -> Lwt.ignore_result @@ be_remove (Storage.be s) selector) stores

    let add_backend_factory engine name factory =
      Logs_lwt.debug (fun m -> m "add_backend_factory : %s" name) >>
      MVar.guarded engine 
      @@ fun self ->
      MVar.return () { self with befs = BackendFactoryMap.add name factory self.befs }


    let add_security engine security = 
      Logs_lwt.debug (fun m -> m "add_backend_security") >>
      MVar.guarded engine 
      @@ fun self ->
      MVar.return () { self with sec = Some security }

    let is_secure engine = 
      MVar.read engine >>= 
      (fun state -> 
         match state.sec with
         | Some _ -> Lwt.return true
         | None -> Lwt.return false
      )


  end
end


