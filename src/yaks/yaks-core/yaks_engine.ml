open Yaks_types
open Yaks_property
open Yaks_access
open Yaks_storage
open Yaks_be 
open Apero.LwtM.InfixM

module SEngine = struct

  module type S = sig 
    type t 
    type subscription_pusher = Yaks_types.SubscriberId.t -> (Yaks_types.Path.t * Yaks_types.Value.t) list -> unit Lwt.t

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

    val create_subscriber : t -> Access.Id.t -> Selector.t -> bool -> subscription_pusher -> SubscriberId.t Lwt.t  
    val remove_subscriber : t -> Access.Id.t -> SubscriberId.t -> unit Lwt.t  
  end

  module Make (MVar: Apero.MVar) = struct

    module StorageMap  = Map.Make (Storage.Id)
    module AccessMap = Map.Make (Access.Id)
    module SubscriberMap = Map.Make (SubscriberId)

    type subscription_pusher = Yaks_types.SubscriberId.t -> (Yaks_types.Path.t * Yaks_types.Value.t) list -> unit Lwt.t
    
    type subscription = 
      { selector : Selector.t 
      ; pusher : subscription_pusher
      ; is_push : bool }

    type state = 
      { backends : (module Backend) list 
      ; stores : Storage.t StorageMap.t
      ; accs : Access.t AccessMap.t 
      ; subs : subscription SubscriberMap.t} 

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
        ; accs = AccessMap.empty 
        ; subs = SubscriberMap.empty }

    (** Subscribers management *)
     let create_subscriber engine _ (* access  *) (selector:Selector.t) (is_push: bool) (pusher:subscription_pusher) =       
      (* @TODO: Should check that we can really subscribe to this.... *)
      let sid = SubscriberId.next_id () in 
      let sub = {selector; pusher; is_push} in 
      (MVar.guarded engine 
      @@ fun self ->         
        let subs' = SubscriberMap.add sid sub self.subs in 
        MVar.return () {self with subs = subs'} )
      >|= fun () -> sid

    let remove_subscriber engine _ (* access*) sid = 
      (MVar.guarded engine 
      @@ fun self ->         
        let subs' = SubscriberMap.remove sid self.subs in 
        MVar.return () {self with subs = subs'} )


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
      let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: dispose_access %s" (Access.Id.to_string access_id)) in
      MVar.guarded engine @@ fun self ->         
        let access = AccessMap.find_opt access_id self.accs in 
        let subs' = (match access with 
          | Some a ->         
            (let asubs = Access.subscribers a in 
            let rec remove_subs smap subs = match subs with 
              | [] -> smap
              | h::tl -> remove_subs (SubscriberMap.remove h smap) tl
            in remove_subs self.subs asubs)
          | None -> self.subs) 
        in                    
        MVar.return () 
        {self with accs =  AccessMap.remove access_id self.accs ; subs = subs'}


    (****************************)
    (*   Storages management    *)
    (****************************)
    let find_compatible_backend (self:state) properties =
      List.find_opt (fun be ->
          let module BE = (val be: Backend) in
          let _ = Logs_lwt.debug (fun m -> m "[YE]:    try Backend %s" BE.to_string) in
          Yaks_property.not_conflicting properties BE.properties) self.backends

    let create_storage engine ?alias path properties =
      MVar.guarded engine
      @@ fun (self:state) ->
      if not @@ StorageMap.exists (fun _ s -> Storage.is_conflicting s path) self.stores then
        match find_compatible_backend self properties with
        | Some be ->
          let module BE = (val be : Backend) in
          let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: create_storage %s {%s} using Backend %s" (Path.to_string path) (string_of_properties properties) (BE.to_string)) in
          let%lwt store = BE.create_storage ?alias path properties in
          MVar.return (store) {self with stores = (StorageMap.add (Storage.id store) store self.stores)}
        | None ->
          let%lwt _ = Logs_lwt.err (fun m -> m "[YE]: create_storage %s {%s} failed: no compatible backend" (Path.to_string path) (string_of_properties properties)) in
          Lwt.fail @@ YException (`NoCompatibleBackend (`Msg (string_of_properties properties)))
      else
        Lwt.fail @@ YException (`ConflictingStorage (`Msg (Path.to_string path)))

    let get_storage engine storage_id =
      MVar.read engine >|= fun self -> StorageMap.find_opt storage_id self.stores

    let dispose_storage engine storage_id =
      let _ = Logs_lwt.debug (fun m -> m "[YE]: dispose_storage") in
      MVar.guarded engine
      @@ fun self ->
      let open Apero.Option.Infix in
      let _ = StorageMap.find_opt storage_id self.stores |> function | Some _ -> print_endline "!!!!! FOUND" | None -> print_endline "!!!! NOT_FOUND" in
      let%lwt _ = StorageMap.find_opt storage_id self.stores >== Storage.dispose >?= Lwt.return_unit in
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

     let notify_subscriber subs path value = 
      SubscriberMap.iter 
        (fun sid sub -> 
          if Selector.is_matching_path path sub.selector then Lwt.ignore_result (sub.pusher sid [(path, value)]) 
          else ()) subs
          
    let put (engine: t) access_id (selector:Selector.t) (value:Value.t) =
      let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: put") in
      MVar.read engine 
      >>= fun self ->      
      let%lwt _ = check_access self access_id selector in
      (* TODO: Potentially here we need to compare two selectors... That measn 
      that we need to agree when the selectors are equivalent. For the time being
      when there is a put of a selector I do notify based on matching on the Path that
      prefixes the selector. This is far from being pefect, but at least a starting point *)
      let _ = match Selector.as_unique_path selector with 
      | Some p -> Lwt.return @@ notify_subscriber self.subs p value 
      | _ -> Logs_lwt.warn (fun m -> m "Unable to extract path from seletor to notify subscribers") 
      in
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

  end
end


