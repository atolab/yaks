open Yaks_types
open Yaks_property
open Yaks_access
open Yaks_storage
open Yaks_be 
open Apero.LwtM.InfixM

module SEngine = struct

  module type S = sig 
    type t 

    val make : unit -> t 

    val add_backend : t -> (module Backend) -> unit Lwt.t

    val create_access : t  -> ?alias:string -> Path.t -> int64 ->  Access.t Lwt.t
    val get_access : t -> Access.Id.t -> Access.t option Lwt.t
    val dispose_access : t -> Access.Id.t -> unit Lwt.t

    val create_storage : t -> ?alias:string -> Path.t -> properties -> Storage.t Lwt.t 
    val get_storage : t -> Storage.Id.t -> Storage.t option Lwt.t
    val dispose_storage : t -> Storage.Id.t -> unit Lwt.t

    val get : t -> Access.Id.t -> Selector.t -> (Path.t * Value.t) list  Lwt.t
    val put : t -> Access.Id.t -> Selector.t -> Value.t -> unit Lwt.t
    val put_delta : t -> Access.Id.t -> Selector.t -> Value.t -> unit Lwt.t
    val remove : t -> Access.Id.t -> Selector.t -> unit Lwt.t

    val create_subscriber : t -> Path.t -> Selector.t -> bool -> SubscriberId.t Lwt.t  
  end

  module Make (MVar: Apero.MVar) = struct

    module StorageMap  = Map.Make (Storage.Id)
    module AccessMap = Map.Make (Access.Id)


    type state = 
      { backends : (module Backend) list 
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
        { backends = []
        ; stores = StorageMap.empty
        ; accs = AccessMap.empty }



    (**************************)
    (*   Backends management  *)
    (**************************)
    let add_backend engine be =
      let module BE = (val be: Backend) in 
      Logs_lwt.debug (fun m -> m "add_backend : %s" BE.to_string) >>
      MVar.guarded engine 
      @@ fun self ->
      MVar.return () { self with backends = be :: self.backends }


    (**************************)
    (*   Access management    *)
    (**************************)
    let create_access engine ?alias path cache_size = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: create_access path: %s  alias: %s" (Path.to_string path) (Apero.Option.get_or_default alias "-")) in
      let access = Access.make ?alias path cache_size in       
      MVar.guarded engine @@ fun self ->
      MVar.return access { self with accs = AccessMap.add (Access.id access) access self.accs }

    let get_access engine access_id =
      MVar.read engine >|= fun self ->
      AccessMap.find_opt access_id self.accs

    let dispose_access engine access_id =
      MVar.guarded engine @@ fun self ->
      MVar.return () {self with accs =  AccessMap.remove access_id self.accs }


    (****************************)
    (*   Storages management    *)
    (****************************)
    let find_compatible_backend (self:state) properties =
      List.find_opt (fun be ->
          let module BE = (val be: Backend) in
          Yaks_property.is_subset properties BE.properties) self.backends

    let create_storage engine ?alias path properties =
      MVar.guarded engine
      @@ fun (self:state) ->
      if not @@ StorageMap.exists (fun _ s -> Storage.is_conflicting s path) self.stores then
        match find_compatible_backend self properties with
        | Some be ->
          let module BE = (val be : Backend) in
          let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: create_storage %s {%s} using Backend %s" (Path.to_string path) (string_of_properties properties) (BE.to_string)) in
          let store = BE.create_storage ?alias path properties in
          MVar.return (store) {self with stores = (StorageMap.add (Storage.id store) store self.stores)}
        | None ->
          Lwt.fail @@ YException (`NoCompatibleBackend (`Msg (string_of_properties properties)))
      else
        Lwt.fail @@ YException (`ConflictingStorage (`Msg (Path.to_string path)))

    let get_storage engine storage_id =
      MVar.read engine >|= fun self -> StorageMap.find_opt storage_id self.stores

    let dispose_storage engine storage_id =
      MVar.guarded engine
      @@ fun self ->
      let self' = {self with stores =  StorageMap.remove storage_id self.stores } in
      MVar.return () self'

    (*****************************)
    (*   Key/Value operations    *)
    (*****************************)
    let check_access (self:state) access_id selector =
      match AccessMap.find_opt access_id self.accs with
      | Some access ->
        if Access.is_covering access selector then
          Lwt.return access
        else 
          Lwt.fail @@ YException (`Forbidden (`Msg  (Printf.sprintf "%s cannot access %s" (Access.to_string access) (Selector.to_string selector))))
      | None -> Lwt.fail @@ YException (`UnknownAccess (`Msg (Access.Id.to_string access_id)))

    let get_matching_stores (self:state) (selector: Selector.t) =
      StorageMap.filter
        (fun _ store -> Storage.is_covering store selector)
        self.stores
      |> StorageMap.bindings

    let get engine access_id selector =
      let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: get %s" (Selector.to_string selector)) in
      MVar.read engine 
      >>= fun self ->
      let%lwt _ = check_access self access_id selector in
      get_matching_stores self selector
      |> List.map (fun (_,store) -> Storage.get store selector)
      |> Apero.LwtM.flatten
      >|= List.concat
    (* TODO? If in the future we accept Storages with conflicting paths,
       there might be duplicate keys from different Storages in this result.
       Shall we remove duplicates?? *)

    let put (engine: t) access_id (selector:Selector.t) (value:Value.t) =
      let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: put") in
      MVar.read engine 
      >>= fun self ->      
      let%lwt _ = check_access self access_id selector in
      get_matching_stores self selector
      |> List.map (fun (_,store) -> Storage.put store selector value)
      |> Lwt.join

    let put_delta engine access_id selector value = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: put_delta") in
      MVar.read engine 
      >>= fun self ->
      let%lwt _ = check_access self access_id selector in
      get_matching_stores self selector
      |> List.map (fun (_,store) -> Storage.put_delta store selector value)
      |> Lwt.join

    let remove engine access_id selector = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: remove") in
      MVar.read engine 
      >>= fun self ->
      let%lwt _ = check_access self access_id selector in
      get_matching_stores self selector
      |> List.map (fun (_,store) -> Storage.remove store selector) 
      |> Lwt.join

    (*******************************)
    (*   Subscribers management    *)
    (*******************************)
    (* TODO !! *)
    let create_subscriber _ _ _ _ = Lwt.return @@  SubscriberId.next_id ()

  end
end


