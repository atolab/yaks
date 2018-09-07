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
    val create_access : t  -> ?alias:string -> Path.t -> int64 ->  Access.t Lwt.t
    val get_access : t -> Access.Id.t -> Access.t option Lwt.t
    val dispose_access : t -> Access.Id.t -> unit Lwt.t

    val create_storage : t -> ?alias:string -> Path.t -> Property.t list -> Storage.t Lwt.t 
    val get_storage : t -> Storage.Id.t -> Storage.t option Lwt.t
    val dispose_storage : t -> Storage.Id.t -> unit Lwt.t

    val create_subscriber : t -> Path.t -> Selector.t -> bool -> SubscriberId.t Lwt.t  

    val get : t -> Access.Id.t -> Selector.t -> (string * Value.t) list  Lwt.t

    val put : t -> Access.Id.t -> Selector.t -> Value.t -> unit Lwt.t
    val put_delta : t -> Access.Id.t -> Selector.t -> Value.t -> unit Lwt.t

    val remove : t -> Access.Id.t -> Selector.t -> unit Lwt.t

    val add_backend_factory : t -> string -> (module BackendFactory) -> unit Lwt.t

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
      ; accs : Access.t AccessMap.t } 

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
        ; accs = AccessMap.empty }



    let get_matching_stores (self:state) (access_id : Access.Id.t) (selector: Selector.t) =
      match AccessMap.find_opt access_id self.accs with 
      | Some access -> 
        let apath = Access.path access in 
        if Path.is_prefix apath (Selector.path selector) then
          StorageMap.filter
            (fun _ (s : Storage.t) -> Path.is_prefix (Storage.path s) (Selector.path selector))
            self.stores
        else
          let _ = Logs_lwt.debug (fun m -> m "[YE];   Access %s with path %s cannot access %s" (Access.Id.to_string access_id) (Path.to_string @@ Access.path access) (Selector.to_string selector)) in      
          StorageMap.empty
      | None ->
        let _ = Logs_lwt.debug (fun m -> m "[YE];   Access %s not found" (Access.Id.to_string access_id)) in
        StorageMap.empty

    let create_access engine ?alias path cache_size = 

      let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.create_access path: %s  alias: %s" (Path.to_string path) (Apero.Option.get_or_default alias "-")) in
      let access = Access.make ?alias path cache_size in       
      MVar.guarded engine 
      @@ (fun self ->  MVar.return access {self with accs = AccessMap.add (Access.id access) access self.accs })

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
      match get_property Property.Backend.Key.key properties with
      | Some (_,v) ->
        create_storage_with_kind engine ?alias path v
      | None -> 
        let _ = Logs_lwt.debug (fun m -> m "SEngine:  Creating with default Memory Backend\n") in
        create_storage_with_kind engine ?alias path Property.Backend.Value.memory
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
      let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: get %s" (Selector.to_string selector)) in
      MVar.read engine 
      >>= fun self ->  
      let (m_be, o_be) = 
        get_matching_stores self access_id selector 
        |> (fun sm -> print_endline @@ "   -> found stores: "^(StorageMap.cardinal sm |> string_of_int); sm )
        |> StorageMap.partition (fun _ s -> Storage.be_kind s = Yaks_be.Memory) in
      let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]:    matching stores found: memory:%d , other:%d" (StorageMap.cardinal m_be) (StorageMap.cardinal o_be)) in
      (* Try to always resolve Get out of memory back-end if available *)
      match StorageMap.find_first_opt (fun _ -> true) m_be with 
      | Some (_, s) -> be_get (Storage.be s) selector 
      | None ->
        let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]:    %s not found in memory" (Selector.to_string selector)) in
        (match StorageMap.find_first_opt (fun _ -> true) o_be with 
         | Some (_, s) -> be_get (Storage.be s) selector
         | None -> 
           let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]:    %s not found in other storages" (Selector.to_string selector)) in
           Lwt.return [])


    let be_put be selector = 
      let module BE = (val be: Backend) in 
      BE.put selector 

    let put (engine: t) access_id (selector:Selector.t) (value:Value.t) =
      let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: put") in
      MVar.read engine 
      >>= fun self ->      
      get_matching_stores self access_id selector
      |> StorageMap.iter (fun _ (s:Storage.t) -> let _ = be_put (Storage.be s) selector value in ())
    ; Lwt.return_unit


    let be_put_delta be selector =
      let module BE = (val be: Backend) in 
      BE.put_delta selector

    let put_delta engine access_id selector value = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: put_delta") in
      MVar.read engine 
      >>= fun self ->
      get_matching_stores self access_id selector
      |> StorageMap.iter (fun _ s -> let _ = be_put_delta (Storage.be s) selector value in ()) 
    ; Lwt.return_unit

    let be_remove be = 
      let module BE = (val be: Backend) in BE.remove

    let remove engine access_id selector = 
      let%lwt self = MVar.read engine in      
      get_matching_stores self access_id selector
      |> StorageMap.iter (fun _ s -> Lwt.ignore_result @@ be_remove (Storage.be s) selector) 
    ; Lwt.return_unit

    let add_backend_factory engine name factory =
      Logs_lwt.debug (fun m -> m "add_backend_factory : %s" name) >>
      MVar.guarded engine 
      @@ fun self ->
      MVar.return () { self with befs = BackendFactoryMap.add name factory self.befs }

  end
end


