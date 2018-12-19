open Apero
open Yaks_common_errors
open Yaks_types
open Yaks_core_types
open Yaks_be
open Yaks_storage
open Apero.LwtM.InfixM

module YAdminSpace = struct

  module type S = sig 
    type t
    val make : unit -> t

    val login : t -> ClientId.t -> properties -> unit Lwt.t
    val logout : t -> ClientId.t -> unit Lwt.t

    val workspace : t -> ClientId.t -> Path.t -> WsId.t Lwt.t

    val get : t -> ClientId.t -> Selector.t -> (Path.t * Value.t) list  Lwt.t
    val put : t -> ClientId.t -> Path.t -> Value.t -> unit Lwt.t
    val remove : t -> ClientId.t -> Path.t -> unit Lwt.t

    val create_subscriber : t -> ClientId.t -> Selector.t -> bool -> notify_subscriber -> SubscriberId.t Lwt.t  
    val remove_subscriber : t -> ClientId.t -> SubscriberId.t -> unit Lwt.t  

    val get_storages_for_path : t -> Path.t -> Storage.t list Lwt.t
    val get_storages_for_selector : t -> Selector.t -> Storage.t list Lwt.t

    (* TODO: Temporary operations that should be replaced by put/get/remove usage *)
    val add_backend_TMP : t -> (module Backend) -> string -> unit Lwt.t

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
      { backend : (module Backend)
      ; storages : Storage.t StorageMap.t
      }

    type state =
      { yid : Uuid.t  
      ; prefix : string
      ; frontends : frontend FrontendMap.t
      ; backends : backend BackendMap.t
      ; kvs : Value.t KVMap.t
      }

    type t = state MVar.t

    let empty_props_value = Value.PropertiesValue Properties.empty

    let make () =
      let yid = Uuid.make () in
      let prefix = "/yaks/"^(Uuid.to_string yid) in
      let _ = Logs_lwt.debug (fun m -> m "Create Yaks %s admin space\n" prefix) in
      let kvs = KVMap.empty
        |> KVMap.add (Path.of_string "/yaks") empty_props_value
        |> KVMap.add (Path.of_string prefix) empty_props_value
        |> KVMap.add (Path.of_string @@ prefix^"/frontend") empty_props_value
        |> KVMap.add (Path.of_string @@ prefix^"/backend") empty_props_value
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
    let add_backend_TMP admin be id =
      let module BE = (val be: Backend) in
      let beid = BeId.of_string id in
      let backend = { backend=be; storages=StorageMap.empty } in
      Logs_lwt.debug (fun m -> m "[Yadm] add_backend : %s" id) >>
      MVar.guarded admin
      @@ fun self ->
      if BackendMap.mem beid self.backends then
        MVar.return_lwt (Lwt.fail @@ YException (`Forbidden (`Msg ("Already existing backend: "^id)))) self
      else
        let kvs = KVMap.add
          (Path.of_string @@ Printf.sprintf "%s/backend/%s/" self.prefix id)
          (Value.StringValue BE.to_string) self.kvs
        in
        MVar.return () { self with backends = BackendMap.add beid backend self.backends; kvs }

    let remove_backend admin beid = 
      let _ = ignore admin and _ = ignore beid in
      Lwt.fail @@ YException (`InternalError (`Msg ("remove_backend not yet implemented")))


    (**************************)
    (*   Storages management  *)
    (**************************)
    let create_storage admin ?beid stid properties =
      let _ = ignore admin and _ = ignore beid and _ = ignore stid and _ = ignore properties in
      Lwt.fail @@ YException (`InternalError (`Msg ("add_storage not yet implemented")))

    let remove_storage admin beid stid =
      let _ = ignore admin and _ = ignore beid and _ = ignore stid in
      Lwt.fail @@ YException (`InternalError (`Msg ("remove_storage not yet implemented")))

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


    let remove_frontend admin feid = 
      Logs_lwt.debug (fun m -> m "[Yadm] remove_frontend %s" feid) >>
      MVar.guarded admin
      @@ fun self ->
      if not @@ FrontendMap.mem (FeId.of_string feid) self.frontends then
        let _ = Logs_lwt.warn (fun m -> m "[Yadm] remove non-existing frontend: %s" feid) in
        MVar.return () self
      else
        let kvs = KVMap.remove
          (Path.of_string @@ Printf.sprintf "%s/frontend/%s" self.prefix feid) self.kvs
        in
        MVar.return () { self with frontends = FrontendMap.remove (FeId.of_string feid) self.frontends; kvs}


    (**************************)
    (*   Sessions management  *)
    (**************************)
    let login admin (clientid:ClientId.t) properties =
      let feid = FeId.to_string clientid.feid in
      let sid = SessionId.to_string clientid.sid in
      Logs_lwt.debug (fun m -> m "[Yadm] %s/%s: login" feid sid) >>
      MVar.guarded admin
      @@ fun self ->
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
      let feid = FeId.to_string clientid.feid in
      let sid = SessionId.to_string clientid.sid in
      Logs_lwt.debug (fun m -> m "[Yadm] %s/%s: logout" feid sid) >>
      MVar.guarded admin
      @@ fun self ->
      match FrontendMap.find_opt clientid.feid self.frontends with
        | None -> let _ = Logs_lwt.warn (fun m -> m "[Yadm] logout from non-existing frontend: %s" feid) in
          MVar.return () self
        | Some fe ->
          if not @@ SessionMap.mem clientid.sid fe.sessions then
             let _ = Logs_lwt.warn (fun m -> m "[Yadm] logout from non-existing session: %s/%s" feid sid) in
            MVar.return () self
          else
            let fe' = {fe with sessions = SessionMap.remove clientid.sid fe.sessions} in
            let kvs = KVMap.remove
              (Path.of_string @@ Printf.sprintf "%s/frontend/%s/session/%s" self.prefix feid sid)
              self.kvs
            in
            MVar.return () { self with frontends = FrontendMap.add clientid.feid fe' self.frontends; kvs }

    let get_frontend_session self feid sid =
      match FrontendMap.find_opt feid self.frontends with
        | None -> Lwt.fail @@ YException (`InternalError (`Msg ("No frontend with id: "^(FeId.to_string feid))))
        | Some fe -> 
          (match SessionMap.find_opt sid fe.sessions with
          | None -> Lwt.fail @@ YException (`InternalError (`Msg ("No session with id: "^(SessionId.to_string sid))))
          | Some s -> Lwt.return (fe, s))

    let workspace admin (clientid:ClientId.t) path =
      let feid = FeId.to_string clientid.feid in
      let sid = SessionId.to_string clientid.sid in
      Logs_lwt.debug (fun m -> m "[Yadm] %s/%s: workspace %s" feid sid (Path.to_string path)) >>
      MVar.guarded admin
      @@ fun self ->
        let%lwt (fe, s) = get_frontend_session self clientid.feid clientid.sid in
        let wsid = WsId.add s.lastWsid WsId.one in
        let s' = { s with ws = WsMap.add wsid path s.ws; lastWsid = wsid } in
        let fe' = {fe with sessions = SessionMap.add clientid.sid s' fe.sessions} in
        let kvs = KVMap.add
          (Path.of_string @@ Printf.sprintf "%s/frontend/%s/session/%s/workspace/%s" self.prefix feid sid (WsId.to_string wsid))
          (Value.StringValue (Path.to_string path)) self.kvs
        in
        MVar.return wsid { self with frontends = FrontendMap.add clientid.feid fe' self.frontends; kvs}

    let create_subscriber admin (clientid:ClientId.t) selector is_push notifier =
      let feid = FeId.to_string clientid.feid in
      let sid = SessionId.to_string clientid.sid in
      Logs_lwt.debug (fun m -> m "[Yadm] %s/%s: create_subscriber %s" feid sid (Selector.to_string selector)) >>
      MVar.guarded admin 
      @@ fun self ->
        let%lwt (fe, s) = get_frontend_session self clientid.feid clientid.sid in
        let subid = SubscriberId.next_id () in
        let sub = {selector; is_push; notifier} in
        let s' = { s with subs = SubscriberMap.add subid sub s.subs } in
        let fe' = {fe with sessions = SessionMap.add clientid.sid s' fe.sessions} in
        let kvs = KVMap.add
          (Path.of_string @@ Printf.sprintf "%s/frontend/%s/session/%s/subscriber/%s" self.prefix feid sid (SubscriberId.to_string subid))
          (Value.StringValue (Selector.to_string selector)) self.kvs
        in
        MVar.return subid { self with frontends = FrontendMap.add clientid.feid fe' self.frontends; kvs}

    let remove_subscriber admin (clientid:ClientId.t) subid =
      let feid = FeId.to_string clientid.feid in
      let sid = SessionId.to_string clientid.sid in
      Logs_lwt.debug (fun m -> m "[Yadm] %s/%s: remove_subscriber %s" feid sid (SubscriberId.to_string subid)) >>
      MVar.guarded admin 
      @@ fun self ->
        let%lwt (fe, s) = get_frontend_session self clientid.feid clientid.sid in
        let s' = { s with subs = SubscriberMap.remove subid s.subs } in
        let fe' = {fe with sessions = SessionMap.add clientid.sid s' fe.sessions} in
        let kvs = KVMap.remove
          (Path.of_string @@ Printf.sprintf "%s/frontend/%s/session/%s/subscriber/%s" self.prefix feid sid (SubscriberId.to_string subid))
          self.kvs
        in
        MVar.return () { self with frontends = FrontendMap.add clientid.feid fe' self.frontends; kvs}


    (*****************************)
    (* get/put/remove operations *)
    (*****************************)
    let get admin (clientid:ClientId.t) selector =
      let _ = ignore clientid in  (* will be used for access control*)
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
      let%lwt prefix = MVar.read admin >|= fun self -> self.prefix in
      let%lwt properties = 
        let open Value in
        match transcode value Properties_encoding with
        | Ok PropertiesValue p -> Lwt.return p
        | Ok _ -> Lwt.fail @@ YException (`UnsupportedTranscoding (`Msg "Transcoding to Properties didn't return a PropertiesValie"))
        | Error e -> Lwt.fail @@ YException e
      in
      if Astring.is_prefix ~affix:prefix (Path.to_string path) then
        match String.split_on_char '/' @@ Astring.with_range ~first:(String.length prefix) (Path.to_string path) with
        | ["frontend"; feid] -> add_frontend admin feid properties
        | ["backend"; beid]  -> add_backend admin beid properties
        | ["backend"; "auto"; "storage"; stid] -> create_storage admin stid properties
        | ["backend"; beid; "storage"; stid] -> create_storage admin ~beid stid properties
        | _ -> Lwt.fail @@ YException (`InvalidPath (`Msg ("Invalid path on admin space: "^(Path.to_string path))))
      else
        Lwt.fail @@ YException (`InternalError (`Msg ("put on remote Yaks admin not yet implemented")))

    let remove admin (clientid:ClientId.t) path =
      let _ = ignore clientid in  (* will be used for access control*)
      let%lwt prefix = MVar.read admin >|= fun self -> self.prefix in
      if Astring.is_prefix ~affix:prefix (Path.to_string path) then
        match String.split_on_char '/' @@ Astring.with_range ~first:(String.length prefix) (Path.to_string path) with
        | ["frontend"; feid] -> remove_frontend admin feid
        | ["backend"; beid]  -> remove_backend admin beid
        | ["backend"; beid; "storage"; stid] -> remove_storage admin beid stid
        | _ -> Lwt.fail @@ YException (`InvalidPath (`Msg ("Invalid path on admin space: "^(Path.to_string path))))
      else
        Lwt.fail @@ YException (`InternalError (`Msg ("put on remote Yaks admin not yet implemented")))

  end

end
