open Apero
open Yaks_common_errors
open Yaks_types
open Yaks_core_types
open Yaks_be
open Yaks_storage
open Apero.LwtM.InfixM

module AdminSpace = struct

  module type S = sig 
    type t
    val make : Yid.t -> HLC.t -> Zenoh.t option -> t

    val login : t -> ClientId.t -> properties -> unit Lwt.t
    val logout : t -> ClientId.t -> unit Lwt.t

    val covers_path : t -> Path.t -> Path.t option Lwt.t
    val covers_selector : t -> Selector.t -> Selector.t option Lwt.t

    val add_workspace : t -> ClientId.t -> Path.t -> WsId.t Lwt.t

    val get : t -> ClientId.t -> Selector.t -> (Path.t * TimedValue.t) list  Lwt.t
    val put : t -> ClientId.t -> Path.t -> TimedValue.t -> unit Lwt.t
    val remove : t -> ClientId.t -> Path.t -> unit Lwt.t

    val create_subscriber : t -> ClientId.t -> Selector.t -> bool -> notify_subscriber -> SubscriberId.t Lwt.t  
    val remove_subscriber : t -> ClientId.t -> SubscriberId.t -> unit Lwt.t  
    val notify_subscribers : t -> Path.t -> Value.t -> unit Lwt.t

    val create_eval : t -> ClientId.t -> Path.t -> eval_function -> unit Lwt.t
    val remove_eval : t -> ClientId.t -> Path.t -> unit Lwt.t
    val call_evals : t -> ClientId.t -> int -> Selector.t -> (Path.t * Value.t list) list  Lwt.t

    val get_workspace_path : t -> ClientId.t -> WsId.t -> Path.t Lwt.t

    val get_storages_for_path : t -> Path.t -> Storage.t list Lwt.t
    val get_storages_for_selector : t -> Selector.t -> Storage.t list Lwt.t

    (* TODO: Temporary operations that should be replaced by put/get/remove usage *)
    val add_backend_TMP : t -> (module Backend) -> unit Lwt.t
    val add_frontend_TMP : t -> string -> properties -> unit Lwt.t

  end

  module Make (MVar: Apero.MVar) = struct

    module BackendMap = Map.Make (BeId)
    module StorageMap  = Map.Make (StorageId)
    module FrontendMap = Map.Make (FeId)
    module SessionMap = Map.Make (SessionId)
    module WsMap = Map.Make (WsId)
    module SubscriberMap = Map.Make (SubscriberId)
    module EvalMap = Map.Make (Path)
    module KVMap = Map.Make(Path)

    type subscriber =
      { selector : Selector.t
      ; is_push : bool
      ; notifier : notify_subscriber
      ; zenoh_sub : Zenoh.sub option
      }

    type session =
      { properties: properties
      ; ws : Path.t WsMap.t
      ; lastWsid: WsId.t
      ; subs : subscriber SubscriberMap.t
      ; evals : (eval_function * Zenoh.storage option) EvalMap.t
      }

    type frontend =
      { properties : properties
      ; sessions : session SessionMap.t
      }

    type backend =
      { beModule : (module Backend)
      ; storages : (Storage.t * Zenoh.storage option) StorageMap.t
      }

    type state =
      { yid: Yid.t
      ; admin_prefix: string
      ; frontends : frontend FrontendMap.t
      ; backends : backend BackendMap.t
      ; kvs : TimedValue.t KVMap.t
      ; hlc : HLC.t
      ; zenoh : Zenoh.t option
      ; mutable zenoh_storage : Zenoh.storage option             
      }

    type t = state MVar.t

    let local_client: ClientId.t = { feid=(FeId.of_string "Yaks"); sid=(SessionId.of_string "0") }

    let empty_props_value = Value.PropertiesValue Properties.empty

    let kvmap_add path value time =
      let tv: TimedValue.t = { time; value } in
      KVMap.add path tv

    let now hlc = match Lwt.pick [HLC.new_timestamp hlc] |> Lwt.poll with
      | Some t -> t
      | None -> failwith "Internal Error"


    let admin_prefix_local = "/@/local"

    let covers_path admin path =
      MVar.read admin >|= fun self ->
      let pat = Path.to_string path in
      if Astring.is_prefix ~affix:admin_prefix_local pat then
        (* if path starts with /@/local, convert to /@/my_yid *)
        Astring.with_range ~first:(String.length admin_prefix_local) pat
        |> Astring.append self.admin_prefix
        |> Path.of_string
        |> Option.return
      else if Astring.is_prefix ~affix:self.admin_prefix pat then
        (* if path starts with /@/my_yid *)
        Some path
      else
        None

    let covers_selector admin sel =
      MVar.read admin >|= fun self ->
      let pat = Selector.path sel in
      if Astring.is_prefix ~affix:admin_prefix_local pat then
        (* if path starts with /@/local, convert to /@/my_yid *)
        Astring.with_range ~first:(String.length admin_prefix_local) pat
        |> Astring.append self.admin_prefix
        |> Selector.of_string
        |> Option.return
      else if Astring.is_prefix ~affix:self.admin_prefix pat then
        (* if path starts with /@/my_yid *)
        Some sel
      else
        None

    (**************************)
    (*   Backends management  *)
    (**************************)
    let add_backend admin beid properties time =
      let _ = ignore admin and _ = ignore beid and _ = ignore properties and _ = ignore time in
      Lwt.fail @@ YException (`InternalError (`Msg ("add_backend not yet implemented")))

    (* TODO: Temporary operation that should be replaced by put/get/remove usage *)
    let add_backend_TMP admin be =
      let module BE = (val be: Backend) in
      let id = BeId.to_string BE.id in
      let backend = { beModule = be; storages = StorageMap.empty } in
      Logs_lwt.debug (fun m -> m "[Yadm] add_backend : %s" id) >>
      MVar.guarded admin
      @@ fun self ->
      if BackendMap.mem BE.id self.backends then
        MVar.return_lwt (Lwt.fail @@ YException (`Forbidden (`Msg ("Already existing backend: "^id)))) self
      else
        let time = now self.hlc in
        let kvs = kvmap_add
          (Path.of_string @@ Printf.sprintf "%s/backend/%s/" self.admin_prefix id)
          (Value.PropertiesValue BE.properties) time self.kvs
        in
        MVar.return () { self with backends = BackendMap.add BE.id backend self.backends; kvs }

    let remove_backend admin beid = 
      let _ = ignore admin and _ = ignore beid in
      Lwt.fail @@ YException (`InternalError (`Msg ("remove_backend not yet implemented")))


    (**************************)
    (*   Storages management  *)
    (**************************)
    let find_compatible_backend backends properties =
      BackendMap.filter (fun beid be ->
        let module BE = (val be.beModule: Backend) in
        let _ = Logs_lwt.debug (fun m -> m "[Yadm]:    try Backend %s (%s)" (BeId.to_string beid) BE.to_string) in
        Properties.not_conflicting properties BE.properties) backends
      |> BackendMap.choose_opt
    
    let incoming_storage_data_handler storage (sample:IOBuf.t) (key:string) = 
      match TimedValue.decode sample with
      | Ok (v, _) -> 
        (match Path.of_string_opt key with 
        | Some path -> 
          let%lwt _ = Logs_lwt.warn (fun m -> m "[YAdm]: Inserting remote update for key %s" key) in 
          Storage.put storage path v
        | _ -> 
          let%lwt _ = Logs_lwt.warn (fun m -> m "[YAdm]: Received data for key %s which I cannot store" key) 
          in Lwt.return_unit)      
      | Error e ->
        let%lwt _ = Logs_lwt.debug (fun m -> m "[YAdm]: Failed to decode TimedValue.t to be stored for key %s: %s" key (Atypes.show_error e)) in
        Lwt.return_unit
    
    let incoming_storage_query_handler storage resname predicate = 
      let s = if predicate = "" then resname else resname ^"?"^predicate in 
      let%lwt _ = Logs_lwt.debug (fun m -> m "[YAdm]: Handling remote query on storage for %s?%s" resname predicate) in 
      match Selector.of_string_opt s with 
      | Some selector ->  
        let%lwt kvs = Storage.get storage selector in
        let evs = List.map
          (fun (path,value) ->
            let spath = Path.to_string path in
            let buf = Result.get  (TimedValue.encode value (IOBuf.create ~grow:4096 4096)) in
            (spath, IOBuf.flip buf)) kvs in
        Lwt.return evs 
      | _ -> 
        let%lwt _ = Logs_lwt.debug (fun m -> m "[YAdm]: Unable to resolve query for %s - not a Selector" s) in 
        Lwt.return []

    let create_zenoh_storage zenoh selector storage = 
      Zenoh.storage (Selector.to_string selector) (incoming_storage_data_handler storage) (incoming_storage_query_handler storage) zenoh 

    let create_storage admin ?beid stid properties time =
      (* get "selector" from properties *)
      let%lwt selector = match Properties.find_opt "selector" properties with
        | None -> Lwt.fail @@ YException (`InternalError (`Msg ("create_storage "^stid^" without 'selector' in properties")))
        | Some s -> (match Selector.of_string_opt s with 
          | None -> Lwt.fail @@ YException (`InternalError (`Msg ("create_storage "^stid^" with invalid 'selector': "^s)))
          | Some s' -> Lwt.return s')
      in
      let storageId = StorageId.of_string stid in
      Logs_lwt.debug (fun m -> m "[Yadm] create_storage %s on %s" stid (Selector.to_string selector)) >>
      MVar.guarded admin
      @@ fun self ->
      (* get backend from beid or a compatible one if beid is not set *)
      let%lwt (beid', be) = match beid with
        | None -> (match find_compatible_backend self.backends properties with
          | Some (id, be) -> Lwt.return (id, be)
          | None -> Lwt.fail @@ YException (`InternalError (`Msg ("create_storage "^stid^" failed: no comatible backend ")))
          )
        | Some s -> let id = BeId.of_string s in
          (match BackendMap.find_opt id self.backends with
          | Some be -> Lwt.return (id, be)
          | None -> Lwt.fail @@ YException (`InternalError (`Msg ("create_storage "^stid^" failed: backend not found: "^s)))
        )
      in
      (* check if storage with this id doen's already exists*)
      if StorageMap.mem storageId be.storages then
        Lwt.fail @@ YException (`InternalError (`Msg (
          Printf.sprintf "create_storage %s on %s failed: storage with same id already exists" stid (BeId.to_string beid'))))
      else
        (* create storage (in backend and in Zenoh) and add it to self state *)
        let module BE = (val be.beModule: Backend) in
        let%lwt _ = Logs_lwt.debug (fun m -> m "[Yadm]: create_storage %s using Backend %s" stid (BE.to_string)) in
        let%lwt storage = BE.create_storage selector properties in
        let%lwt zenoh_storage =  
          match self.zenoh with 
          | Some zenoh ->
            create_zenoh_storage zenoh selector storage  >|= fun storage -> Some storage            
          | None -> Lwt.return None 
        in
        let be' = { be with storages = StorageMap.add storageId (storage, zenoh_storage) be.storages } in
        let kvs = kvmap_add
          (Path.of_string @@ Printf.sprintf "%s/backend/%s/storage/%s" self.admin_prefix (BeId.to_string beid') stid)
          (Value.PropertiesValue properties) time self.kvs
        in
        MVar.return () { self with backends = BackendMap.add beid' be' self.backends; kvs}


    let remove_storage admin beid stid =
      let beid' = BeId.of_string beid in
      let stid' = StorageId.of_string stid in
      Logs_lwt.debug (fun m -> m "[Yadm] remove_storage %s/%s" beid stid) >>
      MVar.guarded admin
      @@ fun self ->
      let%lwt be = match BackendMap.find_opt beid' self.backends with
        | None -> Lwt.fail @@ YException (`InternalError (`Msg ("No backend with id: "^beid)))
        | Some be -> Lwt.return be
      in
      match StorageMap.find_opt stid' be.storages with
      | Some (storage, zenoh_storage) ->
        Storage.dispose storage >>
        let be' = { be with storages = StorageMap.remove stid' be.storages } in
        let kvs = KVMap.remove (Path.of_string @@ Printf.sprintf "%s/backend/%s/storage/%s" self.admin_prefix beid stid) self.kvs in
        let open Apero.Option.Infix in 
        let _ = self.zenoh 
        >>= fun zenoh -> 
          zenoh_storage >|= fun storage ->  Zenoh.unstore storage zenoh 
        in 
        MVar.return () { self with backends = BackendMap.add beid' be' self.backends; kvs}
      | None -> 
        Logs_lwt.debug (fun m -> m "[Yadm] storage %s/%s not found... ignore remove request" beid stid) >>
        MVar.return () self


    let get_storages_for_path admin path =
      (* @TODO: depending on quorum, use Storage.covers_fully *)
      let get_matching_storages backend =
        StorageMap.filter (fun _ (store, _) -> Storage.covers_partially store @@ Selector.of_path path) backend.storages
        |> StorageMap.bindings |> List.map (fun (_,(s, _)) -> s)
      in
      MVar.read admin >|= (fun self ->
        BackendMap.fold (fun _ backend l -> (get_matching_storages backend)::l) self.backends [] |> List.concat)

    let get_storages_for_selector admin sel =
      (* @TODO: depending on quorum, use Storage.covers_fully *)
      let get_matching_storages backend =
        StorageMap.filter (fun _ (store, _) -> Storage.covers_partially store sel) backend.storages
        |> StorageMap.bindings |> List.map (fun (_,(s, _)) -> s)
      in
      MVar.read admin >|= (fun self ->
        BackendMap.fold (fun _ backend l -> (get_matching_storages backend)::l) self.backends [] |> List.concat)


    (***************************)
    (*   Frontends management  *)
    (***************************)
    let add_frontend admin feid properties time =
      Logs_lwt.debug (fun m -> m "[Yadm] add_frontend %s" feid) >>
      MVar.guarded admin
      @@ fun self ->
      if FrontendMap.mem (FeId.of_string feid) self.frontends then
        MVar.return_lwt (Lwt.fail @@ YException (`InternalError (`Msg ("Already existing frontend with id: "^feid)))) self
      else
        let fe = { properties; sessions = SessionMap.empty } in
        let kvs = kvmap_add
          (Path.of_string @@ Printf.sprintf "%s/frontend/%s" self.admin_prefix feid)
          (Value.PropertiesValue properties) time self.kvs
        in
        MVar.return () { self with frontends = FrontendMap.add (FeId.of_string feid) fe self.frontends; kvs}

    let add_frontend_TMP admin feid properties =
      MVar.read admin >>= (fun self ->
        add_frontend admin feid properties (now self.hlc))

    let remove_frontend admin feid = 
      Logs_lwt.debug (fun m -> m "[Yadm] remove_frontend %s" feid) >>
      MVar.guarded admin
      @@ fun self ->
      if not @@ FrontendMap.mem (FeId.of_string feid) self.frontends then
        let _ = Logs_lwt.warn (fun m -> m "[Yadm] remove non-existing frontend: %s" feid) in
        MVar.return () self
      else
        let kvs = KVMap.remove (Path.of_string @@ Printf.sprintf "%s/frontend/%s" self.admin_prefix feid) self.kvs in
        MVar.return () { self with frontends = FrontendMap.remove (FeId.of_string feid) self.frontends; kvs}


    (**************************)
    (*   Sessions management  *)
    (**************************)
    let login admin (clientid:ClientId.t) properties =
      Logs_lwt.debug (fun m -> m "[Yadm] %s: login" (ClientId.to_string clientid)) >>
      MVar.guarded admin
      @@ fun self ->
      let feid = FeId.to_string clientid.feid in
      let sid = SessionId.to_string clientid.sid in
      match FrontendMap.find_opt clientid.feid self.frontends with
        | None -> MVar.return_lwt (Lwt.fail @@ YException (`InternalError (`Msg ("No frontend with id: "^feid)))) self
        | Some fe ->
          if SessionMap.mem clientid.sid fe.sessions then
            MVar.return_lwt (Lwt.fail @@ YException (`InternalError (`Msg ("Already existing session: "^sid)))) self
          else
            let s = { properties; ws=WsMap.empty; lastWsid=WsId.zero; subs=SubscriberMap.empty; evals=EvalMap.empty } in
            let fe' = {fe with sessions = SessionMap.add clientid.sid s fe.sessions} in
            let kvs = kvmap_add
              (Path.of_string @@ Printf.sprintf "%s/frontend/%s/session/%s" self.admin_prefix feid sid)
              (Value.PropertiesValue properties) (now self.hlc) self.kvs
            in
            MVar.return () { self with frontends = FrontendMap.add clientid.feid fe' self.frontends; kvs}

    let logout admin (clientid:ClientId.t) =
      Logs_lwt.debug (fun m -> m "[Yadm] %s: logout" (ClientId.to_string clientid)) >>
      MVar.guarded admin
      @@ fun self ->
      match FrontendMap.find_opt clientid.feid self.frontends with
        | None -> let _ = Logs_lwt.warn (fun m -> m "[Yadm] logout from non-existing frontend: %s" (FeId.to_string clientid.feid)) in
          MVar.return () self
        | Some fe ->
          if not @@ SessionMap.mem clientid.sid fe.sessions then
             let _ = Logs_lwt.warn (fun m -> m "[Yadm] logout from non-existing session: %s" (ClientId.to_string clientid)) in
            MVar.return () self
          else
            let fe' = {fe with sessions = SessionMap.remove clientid.sid fe.sessions} in
            let spath = Path.of_string @@ Printf.sprintf "%s/frontend/%s/session/%s"
                self.admin_prefix (FeId.to_string clientid.feid) (SessionId.to_string clientid.sid)
            in
            let kvs = KVMap.filter (fun p _ -> not @@ Path.is_prefix ~affix:spath p) self.kvs in
            MVar.return () { self with frontends = FrontendMap.add clientid.feid fe' self.frontends; kvs }

    let get_frontend_session self feid sid =
      match FrontendMap.find_opt feid self.frontends with
        | None -> Lwt.fail @@ YException (`InternalError (`Msg ("No frontend with id: "^(FeId.to_string feid))))
        | Some fe -> 
          (match SessionMap.find_opt sid fe.sessions with
          | None -> Lwt.fail @@ YException (`InternalError (`Msg ("No session with id: "^(SessionId.to_string sid))))
          | Some s -> Lwt.return (fe, s))


    (****************************)
    (*   Workspaces management  *)
    (****************************)
    let add_workspace admin (clientid:ClientId.t) path =
      if Path.is_relative path then
        Lwt.fail @@ YException (`InternalError (`Msg ("Invalid workspace with non-absolute path: "^(Path.to_string path))))
      else
        Logs_lwt.debug (fun m -> m "[Yadm] %s: workspace %s" (ClientId.to_string clientid) (Path.to_string path)) >>
        MVar.guarded admin
        @@ fun self ->
        let%lwt (fe, s) = get_frontend_session self clientid.feid clientid.sid in
        let wsid = WsId.add s.lastWsid WsId.one in
        let s' = { s with ws = WsMap.add wsid path s.ws; lastWsid = wsid } in
        let fe' = {fe with sessions = SessionMap.add clientid.sid s' fe.sessions} in
        let kvs = kvmap_add
          (Path.of_string @@ Printf.sprintf "%s/frontend/%s/session/%s/workspace/%s"
            self.admin_prefix (FeId.to_string clientid.feid) (SessionId.to_string clientid.sid) (WsId.to_string wsid))
          (Value.StringValue (Path.to_string path)) (now self.hlc) self.kvs
        in
        MVar.return wsid { self with frontends = FrontendMap.add clientid.feid fe' self.frontends; kvs}

    let get_workspace_path admin (clientid:ClientId.t) wsid =
      MVar.read admin >>= fun self ->
      let%lwt (_, s) = get_frontend_session self clientid.feid clientid.sid in
      match WsMap.find_opt wsid s.ws with
      | None -> Lwt.fail @@ YException (`InternalError (`Msg ("No workspace with id: "^(WsId.to_string wsid))))
      | Some path -> Lwt.return path

    (*******************************)
    (*   Subscriptions management  *)
    (*******************************)

     let remove_subscriber admin (clientid:ClientId.t) subid =
      Logs_lwt.debug (fun m -> m "[Yadm] %s: remove_subscriber %s" (ClientId.to_string clientid) (SubscriberId.to_string subid)) >>
      MVar.guarded admin 
      @@ fun self ->
        let%lwt (fe, s) = get_frontend_session self clientid.feid clientid.sid in
        let open Apero.Option.Infix in         
        let _ = self.zenoh 
        >>= fun zenoh -> 
          (SubscriberMap.find_opt subid s.subs) 
            >>= fun sub_info -> 
              sub_info.zenoh_sub 
              >|= fun zsub -> Zenoh.unsubscribe zsub zenoh
        in 
        let s' = { s with subs = SubscriberMap.remove subid s.subs } in
        let fe' = { fe with sessions = SessionMap.add clientid.sid s' fe.sessions } in
        let kvs = KVMap.remove
          (Path.of_string @@ Printf.sprintf "%s/frontend/%s/session/%s/subscriber/%s"
            self.admin_prefix (FeId.to_string clientid.feid) (SessionId.to_string clientid.sid) (SubscriberId.to_string subid))
          self.kvs
        in
        MVar.return () { self with frontends = FrontendMap.add clientid.feid fe' self.frontends; kvs}

    let create_zenoh_subscriber admin zenoh clientid subid selector is_push notifier  =
      let sub_mode = if is_push then Zenoh.push_mode else Zenoh.pull_mode in 
      let listener buf path = 
        match TimedValue.decode buf with 
        | Ok (tv, _) ->
          Logs_lwt.debug (fun m -> m "[Yadm] notify subscriber %s for remote update on %s" (ClientId.to_string clientid) path) >>
          notifier subid ~fallback:(remove_subscriber admin clientid) [(Path.of_string path, tv.value)]
        | Error e ->
          let%lwt _ = Logs_lwt.warn (fun m -> m "Error while decoding value received for subscription: \n%s" @@ Atypes.show_error e) 
          in Lwt.return_unit
        in
        Zenoh.subscribe (Selector.to_string selector) listener ~mode:sub_mode zenoh

    let create_subscriber admin (clientid:ClientId.t) selector is_push notifier =
      Logs_lwt.debug (fun m -> m "[Yadm] %s: create_subscriber %s" (ClientId.to_string clientid) (Selector.to_string selector)) >>
      MVar.guarded admin 
      @@ fun self ->
        let%lwt (fe, s) = get_frontend_session self clientid.feid clientid.sid in
        let subid = SubscriberId.next_id () in
        let%lwt zenoh_sub = match self.zenoh with 
          | Some zenoh -> 
            let%lwt zsub = create_zenoh_subscriber admin zenoh clientid subid selector is_push notifier in
            Lwt.return @@ Some zsub
          | None -> Lwt.return None 
        in 
        let sub = {selector; is_push; notifier; zenoh_sub} in
        let s' = { s with subs = SubscriberMap.add subid sub s.subs } in
        let fe' = { fe with sessions = SessionMap.add clientid.sid s' fe.sessions } in
        let kvs = kvmap_add
          (Path.of_string @@ Printf.sprintf "%s/frontend/%s/session/%s/subscriber/%s"
            self.admin_prefix (FeId.to_string clientid.feid) (SessionId.to_string clientid.sid) (SubscriberId.to_string subid))
          (Value.StringValue (Selector.to_string selector)) (now self.hlc) self.kvs
        in
        MVar.return subid { self with frontends = FrontendMap.add clientid.feid fe' self.frontends; kvs}

   
    let notify_subscribers admin path value =
      let iter_session feid sid s = SubscriberMap.iter
        (fun subid sub ->
          if Selector.is_matching_path path sub.selector then
            let clientid: ClientId.t = {feid; sid} in
            Lwt.ignore_result (
              Logs_lwt.debug (fun m -> m "[Yadm] notify subscriber %s for local update on %s" (ClientId.to_string clientid) (Path.to_string path)) >>
              sub.notifier subid ~fallback:(remove_subscriber admin clientid) [(path, value)])
          else ()) s.subs
      in
      let iter_frontend feid fe = SessionMap.iter (fun sid session -> iter_session feid sid session) fe.sessions in
      MVar.read admin >|= (fun self ->
      FrontendMap.iter (fun feid fe -> iter_frontend feid fe) self.frontends)

    (*****************************)
    (*     evals management      *)
    (*****************************)
    let remove_eval admin (clientid:ClientId.t) path =
      Logs_lwt.debug (fun m -> m "[Yadm] %s: remove_eval %s" (ClientId.to_string clientid) (Path.to_string path)) >>
      MVar.guarded admin 
      @@ fun self ->
        let%lwt (fe, s) = get_frontend_session self clientid.feid clientid.sid in
        let s' = { s with evals = EvalMap.remove path s.evals } in
        let fe' = { fe with sessions = SessionMap.add clientid.sid s' fe.sessions } in
        let kvs = KVMap.remove
          (Path.of_string @@ Printf.sprintf "%s/frontend/%s/session/%s/eval/%s"
            self.admin_prefix (FeId.to_string clientid.feid) (SessionId.to_string clientid.sid) (Path.to_string path))
          self.kvs
        in
        MVar.return () { self with frontends = FrontendMap.add clientid.feid fe' self.frontends; kvs}

    let call_evals admin (clientid:ClientId.t) quorum sel =
      let _ = ignore clientid in
      let add_eval p e m = if EvalMap.mem p m then EvalMap.add p (e::EvalMap.find p m) m else EvalMap.add p (e::[]) m in
      let remove_eval_fallback feid sid path =
        remove_eval admin {feid; sid} path >>= fun _ ->
        Lwt.return @@ Value.StringValue
          (Printf.sprintf "Error calling get(%s) on eval(%s): Eval implementer was removed"
          (Selector.to_string sel) (Path.to_string path))
      in
      let rec call_evals path evals quorum = 
        let sel' = Selector.of_path ?predicate:(Selector.predicate sel) ?properties:(Selector.properties sel) ?fragment:(Selector.fragment sel) path in
        match quorum with
        | 0 -> []
        | _ -> ((List.hd evals) path sel')::(call_evals path (List.tl evals) (quorum-1))   (* TODO: call a random eval instead of the 1st in list *)
      in
      MVar.read admin >>= fun self ->
      FrontendMap.fold (fun feid fe m ->
        SessionMap.fold (fun sid s m ->
          EvalMap.fold (fun p ((eval:eval_function), _) m ->
            if Selector.is_matching_path p sel then
              add_eval p (eval ~fallback:(remove_eval_fallback feid sid)) m
            else
              m
          ) s.evals m
        ) fe.sessions m
      ) self.frontends EvalMap.empty
      |> EvalMap.bindings
      |> List.map (fun (p,l) -> call_evals p l (max quorum @@ List.length l) |> LwtM.flatten >>= fun l' -> Lwt.return (p,l'))
      |> LwtM.flatten

    let incoming_eval_data_handler _ resname =
      let%lwt _ = Logs_lwt.warn (fun m -> m "[YAdm]: Received pushed data for eval %s - Ignore it!!" resname) in
      Lwt.return_unit (* Eval should never get value "put" *)

    let incoming_eval_query_handler evaluator resname predicate = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "[YAdm]: Handling remote Zenoh query on eval for '%s' '%s'" resname predicate) in
      if Astring.is_prefix ~affix:"+" resname then
        let resname = Astring.with_range ~first:2 resname in
        let s = if String.length predicate = 0 then resname else resname ^"?"^predicate in
        match Selector.of_string_opt s with
        | Some selector ->
          let open Yaks_fe_sock_codec in
            let%lwt kvss = evaluator selector in
            let kvs = List.map (fun (k, vs) -> (k, List.hd vs)) kvss in
            let evs = List.map
              (fun (path,value) ->
                let spath = Path.to_string path in
                let buf = Result.get  (encode_value value (IOBuf.create ~grow:4096 4096)) in
                (spath, IOBuf.flip buf)) kvs in
            Lwt.return evs 
        | _ -> 
          let%lwt _ = Logs_lwt.debug (fun m -> m "[YAdm]: Unable to run eval for %s - not a Selector" s) in
          Lwt.return []
      else
        let%lwt _ = Logs_lwt.debug (fun m -> m "[YAdm]: Internal error the Zenoh resource name for eval doesn't start with + : %s" resname) in
        Lwt.return []

    
    let create_zenoh_eval zenoh selector evaluator = 
      Zenoh.storage ("+"^(Selector.to_string selector)) incoming_eval_data_handler (incoming_eval_query_handler evaluator) zenoh 
      (* NB:
          - Currently an eval is represented with a storage, once zenoh will support something like evals, we'll
            transition to that abstraction to avoid bu construction the progagation of spurious values.
          - The Zenoh storage selector for eval is the eval's path prefixed with '+'
          - The quorum for remotely resolved eval is currently 1       
      *)

    let create_eval admin (clientid:ClientId.t) path eval =
      Logs_lwt.debug (fun m -> m "[Yadm] %s: create_eval %s" (ClientId.to_string clientid) (Path.to_string path)) >>
      MVar.guarded admin 
      @@ fun self ->
        let%lwt (fe, s) = get_frontend_session self clientid.feid clientid.sid in
          let%lwt zenoh_eval = match self.zenoh with 
          | Some zenoh -> 
            let%lwt ze = create_zenoh_eval zenoh (Selector.of_path path) (call_evals admin clientid 1) in 
            Lwt.return @@ Some ze
          | None -> Lwt.return None
        in 
        let s' = { s with evals = EvalMap.add path (eval, zenoh_eval) s.evals } in
        let fe' = {fe with sessions = SessionMap.add clientid.sid s' fe.sessions} in
        let kvs = kvmap_add
          (Path.of_string @@ Printf.sprintf "%s/frontend/%s/session/%s/eval/%s"
            self.admin_prefix (FeId.to_string clientid.feid) (SessionId.to_string clientid.sid) (Path.to_string path))
          empty_props_value (now self.hlc)  self.kvs
        in
        MVar.return () { self with frontends = FrontendMap.add clientid.feid fe' self.frontends; kvs}


    (*****************************)
    (* get/put/remove operations *)
    (*****************************)
    let get admin (clientid:ClientId.t) selector =
      let _ = ignore clientid in  (* will be used for access control*)
      let%lwt _ = Logs_lwt.debug (fun m -> m "[Yadm] %s: get %s" (ClientId.to_string clientid) (Selector.to_string selector)) in
      match Selector.as_unique_path selector with 
      | Some path ->
        MVar.read admin 
        >|= (fun self -> 
            match KVMap.find_opt path self.kvs with 
            | Some v -> [(path, v)]
            | None -> [])
      | None -> 
        MVar.read admin 
        >|= (fun self ->
            self.kvs
            |> KVMap.filter (fun path _ -> Selector.is_matching_path path selector)
            |> KVMap.bindings)

    let put admin (clientid:ClientId.t) path (tvalue:TimedValue.t) =
      let _ = ignore clientid in  (* will be used for access control*)
      let%lwt _ = Logs_lwt.debug (fun m -> m "[Yadm] %s: put %s" (ClientId.to_string clientid) (Path.to_string path)) in
      let time = tvalue.time in
      let%lwt properties = 
        let open Value in
        match transcode tvalue.value Properties_encoding with
        | Ok PropertiesValue p -> Lwt.return p
        | Ok _ -> Lwt.fail @@ YException (`UnsupportedTranscoding (`Msg "Transcoding to Properties didn't return a PropertiesValie"))
        | Error e -> Lwt.fail @@ YException e
      in
      MVar.read admin >>= (fun self ->
        if Astring.is_prefix ~affix:self.admin_prefix (Path.to_string path) then
          match String.split_on_char '/' @@ Astring.with_range ~first:(String.length self.admin_prefix+1) (Path.to_string path) with
          | ["frontend"; feid] -> add_frontend admin feid properties time
          | ["backend"; "auto"]  -> Lwt.fail @@ YException (`InvalidPath (`Msg ("Creation of backend/auto forbidden (reserved id)")))
          | ["backend"; beid]  -> add_backend admin beid properties time
          | ["backend"; "auto"; "storage"; stid] -> create_storage admin stid properties time
          | ["backend"; beid; "storage"; stid] -> create_storage admin ~beid stid properties time
          | _ -> Lwt.fail @@ YException (`InvalidPath (`Msg ("Invalid path on admin space: "^(Path.to_string path))))
        else
          Lwt.fail @@ YException (`InternalError (`Msg ("put on remote Yaks admin not yet implemented"))))

    let remove admin (clientid:ClientId.t) path =
      let _ = ignore clientid in  (* will be used for access control*)
      let%lwt _ = Logs_lwt.debug (fun m -> m "[Yadm] %s: remove %s" (ClientId.to_string clientid) (Path.to_string path)) in
      MVar.read admin >>= (fun self ->
        if Astring.is_prefix ~affix:self.admin_prefix (Path.to_string path) then
          match String.split_on_char '/' @@ Astring.with_range ~first:(String.length self.admin_prefix+1) (Path.to_string path) with
          | ["frontend"; feid] -> remove_frontend admin feid
          | ["backend"; beid]  -> remove_backend admin beid
          | ["backend"; beid; "storage"; stid] -> remove_storage admin beid stid
          | _ -> Lwt.fail @@ YException (`InvalidPath (`Msg ("Invalid path on admin space: "^(Path.to_string path))))
        else
          Lwt.fail @@ YException (`InternalError (`Msg ("put on remote Yaks admin not yet implemented"))))


    let incoming_admin_storage_data_handler admin (sample:IOBuf.t) (key:string) =
      match TimedValue.decode sample with 
      | Ok (v, _) -> 
        (match Path.of_string_opt key with 
        | Some path -> 
          let%lwt _ = Logs_lwt.warn (fun m -> m "[YAdm]: Inserting remote update for key %s" key) in 
          put admin local_client path v
        | _ -> 
          let%lwt _ = Logs_lwt.warn (fun m -> m "[YAdm]: Received data for key %s which I cannot store" key) 
          in Lwt.return_unit)      
      | _ -> Lwt.return_unit 

    let incoming_admin_query_handler admin resname predicate = 
      let s = if predicate = "" then resname else resname ^"?"^predicate in
      match Selector.of_string_opt s with
      | Some selector ->
        let%lwt _ = Logs_lwt.debug (fun m -> m "[YAdm]: handle remote query for %s" s) in
        let%lwt kvs = get admin local_client selector in
        let%lwt _ = Logs_lwt.debug (fun m -> m "[YAdm]: remote query for %s : replying with %d keys" s (List.length kvs)) in
        let evs = List.map
          (fun (path,value) ->
            let spath = Path.to_string path in
            let buf = Result.get  (TimedValue.encode value (IOBuf.create ~grow:4096 4096)) in
            (spath, IOBuf.flip buf)) kvs in
        Lwt.return evs
      | _ ->
        let%lwt _ = Logs_lwt.debug (fun m -> m "[YAdm]: Unable to resolve query for %s?%s" resname predicate) in
        Lwt.return []

    let make yid hlc zenoh =
      let admin_prefix = "/@/"^(Uuid.to_string yid) in
      let _ = Logs_lwt.debug (fun m -> m "Create Yaks %s admin space\n" admin_prefix) in
      let time = now hlc in
      let kvs = KVMap.empty
        |> kvmap_add (Path.of_string admin_prefix) empty_props_value time
        |> fun kvs -> match zenoh with
          | Some z -> kvmap_add
            (Path.of_string @@ admin_prefix^"/transport/zenoh")
            (Value.PropertiesValue (Zenoh.info z)) time kvs
          | None -> kvs
      in
      let admin =  MVar.create
        { yid
        ; admin_prefix
        ; frontends = FrontendMap.empty
        ; backends = BackendMap.empty
        ; kvs
        ; hlc
        ; zenoh
        ; zenoh_storage = None
        }
      in 
      let zenoh_storage_lwt = match zenoh with  
      | Some z -> 
        let selector = (admin_prefix ^ "/**") in
        let%lwt s = Zenoh.storage selector (incoming_admin_storage_data_handler admin) (incoming_admin_query_handler admin) z in
        Lwt.return @@ Some s
      | None -> Lwt.return None 
      in 
      let _  = 
        MVar.guarded admin 
        @@ fun self -> 
          let%lwt zenoh_storage = zenoh_storage_lwt in 
          MVar.return () {self with zenoh_storage}  
      in admin

  end

end
