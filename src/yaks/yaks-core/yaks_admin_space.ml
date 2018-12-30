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
    val make : Yid.t -> t

    val login : t -> ClientId.t -> properties -> unit Lwt.t
    val logout : t -> ClientId.t -> unit Lwt.t

    val add_workspace : t -> ClientId.t -> Path.t -> WsId.t Lwt.t

    val get : t -> ClientId.t -> Selector.t -> (Path.t * Value.t) list  Lwt.t
    val put : t -> ClientId.t -> Path.t -> Value.t -> unit Lwt.t
    val remove : t -> ClientId.t -> Path.t -> unit Lwt.t

    val create_subscriber : t -> ClientId.t -> Selector.t -> bool -> notify_subscriber -> SubscriberId.t Lwt.t  
    val remove_subscriber : t -> ClientId.t -> SubscriberId.t -> unit Lwt.t  

    val get_workspace_path : t -> ClientId.t -> WsId.t -> Path.t Lwt.t

    val get_storages_for_path : t -> Path.t -> Storage.t list Lwt.t
    val get_storages_for_selector : t -> Selector.t -> Storage.t list Lwt.t

    val notify_subscribers : t -> Path.t -> Value.t -> unit Lwt.t

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
      }

    type session =
     { properties: properties
     ; ws : Path.t WsMap.t
     ; lastWsid: WsId.t
     ; subs : subscriber SubscriberMap.t
     ; evals : get_on_eval EvalMap.t
     }

    type frontend =
      { properties : properties
      ; sessions : session SessionMap.t
      }

    type backend =
      { beModule : (module Backend)
      ; storages : Storage.t StorageMap.t
      }

    type state =
      { yid : Uuid.t  
      ; prefix : string   (* prefix used in admin space: "/yaks/yid" *)
      ; frontends : frontend FrontendMap.t
      ; backends : backend BackendMap.t
      ; kvs : Value.t KVMap.t
      }

    type t = state MVar.t

    let local_client: ClientId.t = { feid=(FeId.of_string "Yaks"); sid=(SessionId.of_string "0") }

    let empty_props_value = Value.PropertiesValue Properties.empty

    let make yid =
      let prefix = "/yaks/"^(Uuid.to_string yid) in
      let _ = Logs_lwt.debug (fun m -> m "Create Yaks %s admin space\n" prefix) in
      let kvs = KVMap.empty
        |> KVMap.add (Path.of_string prefix) empty_props_value
      in
      MVar.create
        { yid
        ; prefix
        ; frontends = FrontendMap.empty
        ; backends = BackendMap.empty
        ; kvs
        }


    (**************************)
    (*   Backends management  *)
    (**************************)
    let add_backend admin beid properties =
      let _ = ignore admin and _ = ignore beid and _ = ignore properties in
      Lwt.fail @@ YException (`InternalError (`Msg ("add_backend not yet implemented")))

    (* TODO: Temporary operation that should be replaced by put/get/remove usage *)
    let add_backend_TMP admin be =
      let module BE = (val be: Backend) in
      let id = BeId.to_string BE.id in
      let backend = { beModule=be; storages=StorageMap.empty } in
      Logs_lwt.debug (fun m -> m "[Yadm] add_backend : %s" id) >>
      MVar.guarded admin
      @@ fun self ->
      if BackendMap.mem BE.id self.backends then
        MVar.return_lwt (Lwt.fail @@ YException (`Forbidden (`Msg ("Already existing backend: "^id)))) self
      else
        let kvs = KVMap.add
          (Path.of_string @@ Printf.sprintf "%s/backend/%s/" self.prefix id)
          (Value.PropertiesValue BE.properties) self.kvs
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


    let create_storage admin ?beid stid properties =
      (* get "path" from properties *)
      let%lwt path = match Properties.find_opt "path" properties with
        | None -> Lwt.fail @@ YException (`InternalError (`Msg ("create_storage "^stid^" without 'path' in properties")))
        | Some p -> (match Path.of_string_opt p with 
          | None -> Lwt.fail @@ YException (`InternalError (`Msg ("create_storage "^stid^" with invalid 'path': "^p)))
          | Some p' -> Lwt.return p')
      in
      let storageId = StorageId.of_string stid in
      Logs_lwt.debug (fun m -> m "[Yadm] create_storage %s on %s" stid (Path.to_string path)) >>
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
        (* create storage and add it to self state *)
        let module BE = (val be.beModule: Backend) in
        let%lwt _ = Logs_lwt.debug (fun m -> m "[Yadm]: create_storage %s using Backend %s" stid (BE.to_string)) in
        let%lwt storage = BE.create_storage path properties in
        let be' = { be with storages = StorageMap.add storageId storage be.storages } in
        let kvs = KVMap.add
          (Path.of_string @@ Printf.sprintf "%s/backend/%s/storage/%s" self.prefix (BeId.to_string beid') stid)
          (Value.PropertiesValue properties) self.kvs
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
      let be' = { be with storages = StorageMap.remove stid' be.storages } in
      let kvs = KVMap.remove (Path.of_string @@ Printf.sprintf "%s/backend/%s/storage/%s" self.prefix beid stid) self.kvs in
      MVar.return () { self with backends = BackendMap.add beid' be' self.backends; kvs}


    let get_storages_for_path admin path =
      let get_matching_storages backend =
        StorageMap.filter (fun _ store -> Storage.is_covering_path store path) backend.storages
        |> StorageMap.bindings |> List.map (fun (_,s) -> s)
      in
      MVar.read admin >|= (fun self ->
        BackendMap.fold (fun _ backend l -> (get_matching_storages backend)::l) self.backends [] |> List.concat)

    let get_storages_for_selector admin selector =
      let get_matching_storages backend =
        StorageMap.filter (fun _ store -> Storage.is_covering_selector store selector) backend.storages
        |> StorageMap.bindings |> List.map (fun (_,s) -> s)
      in
      MVar.read admin >|= (fun self ->
        BackendMap.fold (fun _ backend l -> (get_matching_storages backend)::l) self.backends [] |> List.concat)


    (***************************)
    (*   Frontends management  *)
    (***************************)
    let add_frontend admin feid properties =
      Logs_lwt.debug (fun m -> m "[Yadm] add_frontend %s" feid) >>
      MVar.guarded admin
      @@ fun self ->
      if FrontendMap.mem (FeId.of_string feid) self.frontends then
        MVar.return_lwt (Lwt.fail @@ YException (`InternalError (`Msg ("Already existing frontend with id: "^feid)))) self
      else
        let fe = { properties; sessions = SessionMap.empty } in
        let kvs = KVMap.add
          (Path.of_string @@ Printf.sprintf "%s/frontend/%s" self.prefix feid)
          (Value.PropertiesValue properties) self.kvs
        in
        MVar.return () { self with frontends = FrontendMap.add (FeId.of_string feid) fe self.frontends; kvs}

    let add_frontend_TMP = add_frontend

    let remove_frontend admin feid = 
      Logs_lwt.debug (fun m -> m "[Yadm] remove_frontend %s" feid) >>
      MVar.guarded admin
      @@ fun self ->
      if not @@ FrontendMap.mem (FeId.of_string feid) self.frontends then
        let _ = Logs_lwt.warn (fun m -> m "[Yadm] remove non-existing frontend: %s" feid) in
        MVar.return () self
      else
        let kvs = KVMap.remove (Path.of_string @@ Printf.sprintf "%s/frontend/%s" self.prefix feid) self.kvs in
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
            let s = { properties; ws=WsMap.empty; lastWsid=WsId.zero ;subs=SubscriberMap.empty; evals=EvalMap.empty } in
            let fe' = {fe with sessions = SessionMap.add clientid.sid s fe.sessions} in
            let kvs = KVMap.add
              (Path.of_string @@ Printf.sprintf "%s/frontend/%s/session/%s" self.prefix feid sid)
              (Value.PropertiesValue properties) self.kvs
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
                self.prefix (FeId.to_string clientid.feid) (SessionId.to_string clientid.sid)
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
        let kvs = KVMap.add
          (Path.of_string @@ Printf.sprintf "%s/frontend/%s/session/%s/workspace/%s"
            self.prefix (FeId.to_string clientid.feid) (SessionId.to_string clientid.sid) (WsId.to_string wsid))
          (Value.StringValue (Path.to_string path)) self.kvs
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
    let create_subscriber admin (clientid:ClientId.t) selector is_push notifier =
      Logs_lwt.debug (fun m -> m "[Yadm] %s: create_subscriber %s" (ClientId.to_string clientid) (Selector.to_string selector)) >>
      MVar.guarded admin 
      @@ fun self ->
        let%lwt (fe, s) = get_frontend_session self clientid.feid clientid.sid in
        let subid = SubscriberId.next_id () in
        let sub = {selector; is_push; notifier} in
        let s' = { s with subs = SubscriberMap.add subid sub s.subs } in
        let fe' = {fe with sessions = SessionMap.add clientid.sid s' fe.sessions} in
        let kvs = KVMap.add
          (Path.of_string @@ Printf.sprintf "%s/frontend/%s/session/%s/subscriber/%s"
            self.prefix (FeId.to_string clientid.feid) (SessionId.to_string clientid.sid) (SubscriberId.to_string subid))
          (Value.StringValue (Selector.to_string selector)) self.kvs
        in
        MVar.return subid { self with frontends = FrontendMap.add clientid.feid fe' self.frontends; kvs}

    let remove_subscriber admin (clientid:ClientId.t) subid =
      Logs_lwt.debug (fun m -> m "[Yadm] %s: remove_subscriber %s" (ClientId.to_string clientid) (SubscriberId.to_string subid)) >>
      MVar.guarded admin 
      @@ fun self ->
        let%lwt (fe, s) = get_frontend_session self clientid.feid clientid.sid in
        let s' = { s with subs = SubscriberMap.remove subid s.subs } in
        let fe' = {fe with sessions = SessionMap.add clientid.sid s' fe.sessions} in
        let kvs = KVMap.remove
          (Path.of_string @@ Printf.sprintf "%s/frontend/%s/session/%s/subscriber/%s"
            self.prefix (FeId.to_string clientid.feid) (SessionId.to_string clientid.sid) (SubscriberId.to_string subid))
          self.kvs
        in
        MVar.return () { self with frontends = FrontendMap.add clientid.feid fe' self.frontends; kvs}

    let notify_subscribers admin path value =
      let iter_session s = SubscriberMap.iter
        (fun sid sub -> 
          if Selector.is_matching_path path sub.selector then
            Lwt.ignore_result (sub.notifier sid ~fallback:(remove_subscriber admin local_client) [(path, value)]) 
          else ()) s.subs
      in
      let iter_frontend f = SessionMap.iter (fun _ session -> iter_session session) f.sessions in
      MVar.read admin >|= (fun self ->
      FrontendMap.iter (fun _ fe -> iter_frontend fe) self.frontends)

 

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

    let put admin (clientid:ClientId.t) path value =
      let _ = ignore clientid in  (* will be used for access control*)
      let%lwt _ = Logs_lwt.debug (fun m -> m "[Yadm] %s: put %s" (ClientId.to_string clientid) (Path.to_string path)) in
      let%lwt prefix = MVar.read admin >|= fun self -> self.prefix in
      let%lwt properties = 
        let open Value in
        match transcode value Properties_encoding with
        | Ok PropertiesValue p -> Lwt.return p
        | Ok _ -> Lwt.fail @@ YException (`UnsupportedTranscoding (`Msg "Transcoding to Properties didn't return a PropertiesValie"))
        | Error e -> Lwt.fail @@ YException e
      in
      if Astring.is_prefix ~affix:prefix (Path.to_string path) then
        match String.split_on_char '/' @@ Astring.with_range ~first:(String.length prefix+1) (Path.to_string path) with
        | ["frontend"; feid] -> add_frontend admin feid properties
        | ["backend"; "auto"]  -> Lwt.fail @@ YException (`InvalidPath (`Msg ("Creation of backend/auto forbidden (reserved id)")))
        | ["backend"; beid]  -> add_backend admin beid properties
        | ["backend"; "auto"; "storage"; stid] -> create_storage admin stid properties
        | ["backend"; beid; "storage"; stid] -> create_storage admin ~beid stid properties
        | _ -> Lwt.fail @@ YException (`InvalidPath (`Msg ("Invalid path on admin space: "^(Path.to_string path))))
      else
        Lwt.fail @@ YException (`InternalError (`Msg ("put on remote Yaks admin not yet implemented")))

    let remove admin (clientid:ClientId.t) path =
      let _ = ignore clientid in  (* will be used for access control*)
      let%lwt _ = Logs_lwt.debug (fun m -> m "[Yadm] %s: remove %s" (ClientId.to_string clientid) (Path.to_string path)) in
      let%lwt prefix = MVar.read admin >|= fun self -> self.prefix in
      if Astring.is_prefix ~affix:prefix (Path.to_string path) then
        match String.split_on_char '/' @@ Astring.with_range ~first:(String.length prefix+1) (Path.to_string path) with
        | ["frontend"; feid] -> remove_frontend admin feid
        | ["backend"; beid]  -> remove_backend admin beid
        | ["backend"; beid; "storage"; stid] -> remove_storage admin beid stid
        | _ -> Lwt.fail @@ YException (`InvalidPath (`Msg ("Invalid path on admin space: "^(Path.to_string path))))
      else
        Lwt.fail @@ YException (`InternalError (`Msg ("put on remote Yaks admin not yet implemented")))

  end

end
