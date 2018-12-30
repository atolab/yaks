open Apero
open Yaks_common_errors
open Yaks_types
open Yaks_access
open Yaks_storage
open Yaks_be 
open Apero.LwtM.InfixM

module SEngine = struct

  module type S = sig 
    type t 
    type subscription_pusher =  Yaks_types.SubscriberId.t -> fallback:(Yaks_types.SubscriberId.t -> unit Lwt.t) -> (Yaks_types.Path.t * Yaks_types.Value.t) list -> unit Lwt.t
    type eval_getter = Path.t -> Selector.t -> fallback:(Path.t -> Value.t Lwt.t) -> Value.t Lwt.t

    val make : Zenoh.t -> t 

    val add_backend : t -> (module Backend) -> unit Lwt.t

    val create_access : t  -> Path.t -> properties ->  Access.t Lwt.t
    val get_access : t -> Access.Id.t -> Access.t option Lwt.t
    val dispose_access : t -> Access.Id.t -> unit Lwt.t

    val create_storage : t -> Path.t -> properties -> Storage.t Lwt.t 
    val get_storage : t -> Storage.Id.t -> Storage.t option Lwt.t
    val dispose_storage : t -> Storage.Id.t -> unit Lwt.t

    val get : t -> Access.Id.t -> Selector.t -> (Path.t * Value.t) list  Lwt.t
    val put : t -> Access.Id.t -> Path.t -> Value.t -> unit Lwt.t
    val put_delta : t -> Access.Id.t -> Path.t -> Value.t -> unit Lwt.t
    val remove : t -> Access.Id.t -> Path.t -> unit Lwt.t

    val create_subscriber : t -> Access.Id.t -> Selector.t -> bool -> subscription_pusher -> SubscriberId.t Lwt.t  
    val remove_subscriber : t -> Access.Id.t -> SubscriberId.t -> unit Lwt.t

    val eval : t -> Access.Id.t -> Path.t -> eval_getter -> unit Lwt.t
  end

  module Make (MVar: Apero.MVar) = struct

    module StorageMap  = Map.Make (Storage.Id)
    module ZStoresMap = Map.Make(String)
    module AccessMap = Map.Make (Access.Id)
    module SubscriberMap = Map.Make (SubscriberId)
    module EvalMap = Map.Make (Path)

    type subscription_pusher = SubscriberId.t -> fallback:(SubscriberId.t -> unit Lwt.t) -> (Path.t * Value.t) list -> unit Lwt.t
    
    type subscription = 
      { selector : Selector.t 
      ; pusher : subscription_pusher
      ; is_push : bool }

    type eval_getter = Path.t -> Selector.t -> fallback:(Path.t -> Value.t Lwt.t) -> Value.t Lwt.t

    type state = 
      { backends : (module Backend) list 
      ; stores : Storage.t StorageMap.t
      ; accs : Access.t AccessMap.t 
      ; subs : subscription SubscriberMap.t
      ; evals : (Access.Id.t * eval_getter) EvalMap.t
      ; zenoh : Zenoh.t
      ; zenoh_stores : Zenoh.storage ZStoresMap.t 
      }

    type t = state MVar.t

    (* 
    let create_access engine path cache_size = (engine, Lwt.return @@ Access.Id.next_id ())
    let create_storage engine path properties = (engine, Lwt.return   @@ StorageId.next_id ()  )
    let create_subscriber engine path selector push = (engine, Lwt.return @@  SubscriberId.next_id ()) 
    let add_back_end engine be_name be_module = (engine, Lwt.return_unit)
    *)

    let make zenoh = 
      let _ = Logs_lwt.debug (fun m -> m "Creating Engine\n") in 
      MVar.create 
        { backends = []
        ; stores = StorageMap.empty
        ; accs = AccessMap.empty 
        ; subs = SubscriberMap.empty
        ; evals = EvalMap.empty 
        ; zenoh
        ; zenoh_stores = ZStoresMap.empty }


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
    let create_access engine path properties = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: create_access %s with %s" (Path.to_string path) (Properties.to_string properties)) in
      let access = Access.make path properties in       
      MVar.guarded engine @@ fun self ->
      MVar.return access { self with accs = AccessMap.add (Access.id access) access self.accs }

    let get_access engine access_id =
      MVar.read engine >|= fun self ->
      AccessMap.find_opt access_id self.accs

    let dispose_access engine access_id =
      let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: dispose_access %s" (Access.Id.to_string access_id)) in
      MVar.guarded engine @@ fun self ->
        let accs', subs', evals' = match AccessMap.find_opt access_id self.accs with
        | Some access ->
          let asubs = Access.subscribers access in
          let rec remove_subs smap subs = match subs with 
            | [] -> smap
            | h::tl -> remove_subs (SubscriberMap.remove h smap) tl
          in
          AccessMap.remove access_id self.accs,
          remove_subs self.subs asubs,
          EvalMap.filter (fun _  (aid, _) -> aid <> access_id) self.evals
        | None -> self.accs, self.subs, self.evals
       in
        MVar.return () 
        { self with accs = accs' ; subs = subs' ; evals = evals' }

    (****************************)
    (*   Storages management    *)
    (****************************)
    let find_compatible_backend (self:state) properties =
      List.find_opt (fun be ->
          let module BE = (val be: Backend) in
          let _ = Logs_lwt.debug (fun m -> m "[YE]:    try Backend %s" BE.to_string) in
          Properties.not_conflicting properties BE.properties) self.backends

    let remove_subscriber_no_acc engine sid = 
      (MVar.guarded engine 
      @@ fun self ->         
        let subs' = SubscriberMap.remove sid self.subs in 
        MVar.return () {self with subs = subs'} )
  
    let notify_subscriber engine subs path value = 
      SubscriberMap.iter 
        (fun sid sub -> 
          if Selector.is_matching_path path sub.selector then Lwt.ignore_result (sub.pusher sid ~fallback:(remove_subscriber_no_acc engine) [(path, value)]) 
          else ()) subs

    let get_stores_for_path (self:state) (path: Path.t) =
      StorageMap.filter
        (fun _ store -> Storage.is_covering_path store path)
        self.stores
      |> StorageMap.bindings
 
    let remote_put (engine: t) (path:Path.t) (value:Value.t) =
      let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: put %s" (Path.to_string path)) in
      MVar.read engine 
      >>= fun self ->      
      let _ = Lwt.return @@ notify_subscriber engine self.subs path value in
      get_stores_for_path self path
      |> List.map (fun (_,store) -> Storage.put store path value)
      |> Lwt.join

    let incoming_data_handler engine (data:IOBuf.t) (key:string) = 
      let open Yaks_fe_sock_codec in 
      match decode_value data with 
      | Ok (v, _) -> 
        (match Path.of_string_opt key with 
        | Some path -> 
          let%lwt _ = Logs_lwt.warn (fun m -> m "Inserting remote update for key %s" key) in 
          remote_put engine path v  
        | _ -> 
          let%lwt _ = Logs_lwt.warn (fun m -> m "Received data for key %s which I cannot store" key) 
          in Lwt.return_unit)      
      | _ -> Lwt.return_unit

    let remove_eval engine path =
      (MVar.guarded engine 
      @@ fun self ->         
        let evals' = EvalMap.remove path self.evals in 
        MVar.return () {self with evals = evals'} )

    let get_on_evals engine (selector: Selector.t) =
      let fallback path =
        remove_eval engine path >>= fun _ ->
        Lwt.return @@ Value.StringValue
          (Printf.sprintf "Error calling get(%s) on eval(%s): Access was removed"
          (Selector.to_string selector) (Path.to_string path))
      in
      let call_eval path (_,(eval_getter:eval_getter)) = eval_getter path selector ~fallback
      in
      MVar.read engine >>= fun self ->
      let evals = EvalMap.filter (fun path _ -> Selector.is_matching_path path selector) self.evals in
      EvalMap.mapi call_eval evals |> EvalMap.bindings |> List.map (fun (p,v) -> v >|= (fun v -> (p,v))) |> LwtM.flatten

    let get_stores_for_selector (self:state) (selector: Selector.t) =
      StorageMap.filter
        (fun _ store -> Storage.is_covering_selector store selector)
        self.stores
      |> StorageMap.bindings

    let remote_query_handler promise mlist sample =
      MVar.guarded mlist 
      @@ fun xs -> 
        match sample with 
        | Zenoh.StorageData {stoid; rsn=_; resname; data} ->                    
          (match 
            let open Apero.Result.Infix in 
            let open Yaks_fe_sock_codec in       
            let _ = Logs.debug (fun m -> m ">>> Query Handler Received data for key: %s" resname) in 
            let store_id = IOBuf.hexdump ~separator:":" stoid in 
            let _ = Logs.debug (fun m -> m ">>> Query Handler Received data for key: %s from storage %s" resname store_id) in          
            decode_value data 
            >>= fun (value, _) -> 
              let _ = Logs.debug (fun m -> m ">>> Query Handler parsed data for key: %s" resname) in 
              Ok(store_id, Path.of_string(resname), value)
          with 
          | Ok sample -> MVar.return () (sample::xs)
          | _ -> MVar.return () xs)
        | Zenoh.StorageFinal {stoid; rsn;} -> 
          let store_id = IOBuf.hexdump ~separator:":" stoid in 
          let%lwt _ = Logs_lwt.debug (fun m -> m "QUERY HANDLER RECIEVED FROM STORAGE [%-16s:%02i] FINAL\n%!" (store_id) rsn) in
          MVar.return () xs          
        | Zenoh.ReplyFinal -> 
          let%lwt _ = Logs_lwt.debug (fun m -> m "QUERY HANDLER RECIEVED GLOBAL FINAL\n%!") in
          Lwt.wakeup_later promise xs;
          MVar.return () xs

          
    let consolidate_query_result (qrs:(string * Path.t * Value.t) list) = 
      List.map (fun (_,p,v) -> (p,v)) qrs
      

    let issue_remote_query engine selector =      
      MVar.read engine 
      >>= fun self -> 
        let path = Selector.path selector in 
        let p,r = Lwt.wait () in 
        let mlist = MVar.create [] in 
        let query =  (Option.get_or_default (Selector.predicate selector) "") in
        let%lwt () = Zenoh.query path query (remote_query_handler r mlist) self.zenoh in
        let open Lwt.Infix in 
        p >|= consolidate_query_result

    let serve_remote_query engine selector =
      let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: serving remote query for  %s" (Selector.to_string selector)) in
      MVar.read engine 
      >>= fun self ->
      match Selector.predicate selector with
      | Some q when Apero.Astring.get q 0 = '!' ->
        let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: Remote query -- get %s from evals" (Selector.to_string selector)) in
        get_on_evals engine selector
      | _ ->
        let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: Remote query -- get %s from local storages" (Selector.to_string selector)) in        
        let%lwt ts = 
          get_stores_for_selector self selector
          |> List.map (fun (_,store) -> Storage.get store selector)
          |> Apero.LwtM.flatten
          >|= List.concat
        in 
          let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: sending %d tuples" (List.length ts)) in
          Lwt.return ts

    let query_handler engine resname predicate  =       
      match Selector.of_string_opt  (resname ^ predicate) with 
      | Some selector ->  
        let open Yaks_fe_sock_codec in 
          let%lwt kvs = serve_remote_query engine selector in 
          let evs = List.map 
            (fun (path,value) -> 
              let spath = Path.to_string path in 
              let buf = Result.get  (encode_value value (IOBuf.create ~grow:4096 4096)) in 
              (spath, IOBuf.flip buf)) kvs in 
          Lwt.return evs 
      | _ -> 
        let%lwt _ = Logs_lwt.debug (fun m -> m "Unable to resolve query for %s?%s" resname predicate) in 
        Lwt.return []
            
    let to_zenoh_storage_path path = 
      let p = Path.to_string path in 
      if (String.get p (String.length p - 1)) = '/' then (Printf.sprintf ("%s**") p)
      else Printf.sprintf "%s/**" p 

    let add_zenoh_storage self engine (path:Path.t) =                 
      let spath = to_zenoh_storage_path path in 
        match ZStoresMap.find_opt spath self.zenoh_stores with 
        | Some _ ->  Lwt.return self
        | _ -> 
          let%lwt storage = Zenoh.storage spath (incoming_data_handler engine) (query_handler engine) self.zenoh in       
          let zenoh_stores' = ZStoresMap.add spath storage self.zenoh_stores in 
          Lwt.return {self with zenoh_stores = zenoh_stores'}
      
    let remove_zenoh_storage self path  = 
      let spath = to_zenoh_storage_path path in       
      match ZStoresMap.find_opt spath self.zenoh_stores with 
      | Some storage ->  
        let%lwt () = Zenoh.unstore storage self.zenoh in 
        let zenoh_stores' = ZStoresMap.remove spath self.zenoh_stores in 
        Lwt.return {self with zenoh_stores = zenoh_stores'}
      | _ -> Lwt.return self
    
    let distribute_update path value tx =
      let res = Path.to_string path in
      let buf = IOBuf.create ~grow:8192 8192 in 
      let open Yaks_fe_sock_codec in
      match (encode_value value buf) with 
      | Ok buf -> 
        let buf' = IOBuf.flip buf in 
        Zenoh.write buf' res tx.zenoh 
      | Error e -> Lwt.fail @@ Exception e 
  
    let create_storage engine path properties =
      MVar.guarded engine
      @@ fun (self:state) ->
      if not @@ StorageMap.exists (fun _ s -> Storage.is_conflicting s path) self.stores then
        match find_compatible_backend self properties with
        | Some be ->
          let module BE = (val be : Backend) in
          let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: create_storage %s {%s} using Backend %s" (Path.to_string path) (Properties.to_string properties) (BE.to_string)) in
          let%lwt store = BE.create_storage path properties in          
          let self' = {self with stores = (StorageMap.add (Storage.id store) store self.stores)} in 
          let%lwt self'' = add_zenoh_storage self' engine path in 
          MVar.return (store) self''
        | None ->
          let%lwt _ = Logs_lwt.err (fun m -> m "[YE]: create_storage %s {%s} failed: no compatible backend" (Path.to_string path) (Properties.to_string properties)) in
          Lwt.fail @@ YException (`NoCompatibleBackend (`Msg (Properties.to_string properties)))
      else
        Lwt.fail @@ YException (`ConflictingStorage (`Msg (Path.to_string path)))

    let get_storage engine storage_id =
      MVar.read engine >|= fun self -> StorageMap.find_opt storage_id self.stores

    let dispose_storage engine storage_id =
      let _ = Logs_lwt.debug (fun m -> m "[YE]: dispose_storage") in
      MVar.guarded engine
      @@ fun self ->      
      let _ = StorageMap.find_opt storage_id self.stores |> function | Some _ -> print_endline "!!!!! FOUND" | None -> print_endline "!!!! NOT_FOUND" in
      match StorageMap.find_opt storage_id self.stores with 
      | Some storage ->  
        let%lwt () = Storage.dispose storage in 
        let self' = {self with stores =  StorageMap.remove storage_id self.stores } in
        let%lwt self'' = remove_zenoh_storage self' (Storage.path storage) in  
        MVar.return () self''
      | None -> MVar.return () self
            

    (****************************)
    (*  Subscribers management  *)
    (****************************)
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


    (****************************)
    (*     Eval management      *)
    (****************************)
    let eval engine access_id path eval_getter =
      let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: eval %s" (Path.to_string path)) in
      (MVar.guarded engine 
      @@ fun self ->         
        let evals' = EvalMap.add path (access_id, eval_getter) self.evals in 
        MVar.return () {self with evals = evals'} )



    (*****************************)
    (*   Key/Value operations    *)
    (*****************************)
    let check_access_for_path (self:state) access_id path =
      match AccessMap.find_opt access_id self.accs with
      | Some access ->
        if Access.is_covering_path access path then
          Lwt.return access
        else 
          Lwt.fail @@ YException (`Forbidden (`Msg  (Printf.sprintf "%s cannot access Path %s" (Access.to_string access) (Path.to_string path))))
      | None -> Lwt.fail @@ YException (`UnknownAccess (`Msg (Access.Id.to_string access_id)))

    let check_access_for_selector (self:state) access_id selector =
      match AccessMap.find_opt access_id self.accs with
      | Some access ->
        if Access.is_covering_selector access selector then
          Lwt.return access
        else 
          Lwt.fail @@ YException (`Forbidden (`Msg  (Printf.sprintf "%s cannot access Selector %s" (Access.to_string access) (Selector.to_string selector))))
      | None -> Lwt.fail @@ YException (`UnknownAccess (`Msg (Access.Id.to_string access_id)))


    let get_stores_for_selector (self:state) (selector: Selector.t) =
      StorageMap.filter
        (fun _ store -> Storage.is_covering_selector store selector)
        self.stores
      |> StorageMap.bindings

    let get_on_evals engine (selector: Selector.t) =
      let fallback path =
        remove_eval engine path >>= fun _ ->
        Lwt.return @@ Value.StringValue
          (Printf.sprintf "Error calling get(%s) on eval(%s): Access was removed"
          (Selector.to_string selector) (Path.to_string path))
      in
      let call_eval path (_,(eval_getter:eval_getter)) = eval_getter path selector ~fallback
      in
      MVar.read engine >>= fun self ->
      let evals = EvalMap.filter (fun path _ -> Selector.is_matching_path path selector) self.evals in
      EvalMap.mapi call_eval evals |> EvalMap.bindings |> List.map (fun (p,v) -> v >|= (fun v -> (p,v))) |> LwtM.flatten

    let get engine access_id selector =
      MVar.read engine 
      >>= fun self ->
      match Selector.predicate selector with
      | Some q when Apero.Astring.get q 0 = '!' ->
        let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: get %s from evals" (Selector.to_string selector)) in
        get_on_evals engine selector
      | _ ->
        let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: get %s from storages" (Selector.to_string selector)) in
        let%lwt _ = check_access_for_selector self access_id selector in
        let mstores = get_stores_for_selector self selector in
        let%lwt _ = Logs_lwt.debug (fun m -> m "Fond %d matching stores" (List.length mstores)) in 
        match mstores with 
        | [] -> 
          let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: Did not find matching local stores for get, query network...") in 
          issue_remote_query engine selector 
        | _ as xs -> 
          let covers: bool =  xs 
            |> List.map (fun (_, s) -> Selector.of_path (Storage.path s))
            |> List.map (fun s -> Selector.of_string @@ (Selector.path s) ^ "**")
            |> List.map (fun s -> Selector.covers s selector) 
            |> List.fold_left (fun a b -> a && b) true 
          in 
          let%lwt _ = 
            Logs_lwt.debug (fun m -> m "[YE]: Found %d matching (covering = %b) local stores for get, query network" (List.length xs) covers) in             
          let%lwt local_get = xs
            |> List.map (fun (_,store) -> Storage.get store selector)
            |> Apero.LwtM.flatten
            >|= List.concat in 
          if covers then Lwt.return local_get
          else 
            let%lwt remote_get = issue_remote_query engine selector in 
            Lwt.return (List.append remote_get local_get)              
            

      (* TODO? If in the future we accept Storages with conflicting paths,
        there might be duplicate keys from different Storages in this result.
        Shall we remove duplicates?? *)

          
    let put (engine: t) access_id (path:Path.t) (value:Value.t) =
      let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: put %s" (Path.to_string path)) in
      MVar.read engine 
      >>= fun self ->      
      let%lwt _ = check_access_for_path self access_id path in
      (* TODO: Potentially here we need to compare two selectors... That measn 
      that we need to agree when the selectors are equivalent. For the time being
      when there is a put of a selector I do notify based on matching on the Path that
      prefixes the selector. This is far from being pefect, but at least a starting point *)
      let _ = Lwt.return @@ notify_subscriber engine self.subs path value in
      (get_stores_for_path self path
      |> List.map (fun (_,store) -> Storage.put store path value)
      |> Lwt.join) >>= fun () -> distribute_update path value self

    let put_delta engine access_id path value = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: put_delta %s" (Path.to_string path)) in
      MVar.read engine 
      >>= fun self ->
      let%lwt _ = check_access_for_path self access_id path in
      get_stores_for_path self path
      |> List.map (fun (_,store) -> Storage.put_delta store path value)
      |> Lwt.join

    let remove engine access_id path = 
      MVar.read engine 
      >>= fun self ->
      let%lwt _ = check_access_for_path self access_id path in
      match EvalMap.find_opt path self.evals with
      | Some _ ->
        let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: remove %s (eval)" (Path.to_string path)) in
        (MVar.guarded engine 
        @@ fun self ->         
          let evals' = EvalMap.remove path self.evals in 
          MVar.return () {self with evals = evals'} )
      | None ->
        let%lwt _ = Logs_lwt.debug (fun m -> m "[YE]: remove %s" (Path.to_string path)) in
        get_stores_for_path self path
        |> List.map (fun (_,store) -> Storage.remove store path) 
        |> Lwt.join
  end
end


