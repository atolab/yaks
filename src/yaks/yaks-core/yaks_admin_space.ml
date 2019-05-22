open Apero
open Yaks_common_errors
open Yaks_types
open Yaks_core_types
open Yaks_be
open Yaks_storage
open Apero.LwtM.InfixM

module AdminSpace = struct

  
    module BackendMap = Map.Make (BeId)
    module StorageMap  = Map.Make (StorageId)
    module FrontendMap = Map.Make (FeId)
    module SessionMap = Map.Make (SessionId)
    module WsMap = Map.Make (WsId)
    module SubscriberMap = Map.Make (SubscriberId)
    module EvalMap = Map.Make (Path)
    module KVMap = Map.Make(Path)

    module ZUtils = Yaks_zenoh_utils

    type eval_call = Selector.t -> Value.t Lwt.t

    type subscriber =
      { selector : Selector.t
      ; is_push : bool
      ; notify_call : Path.t -> change list -> unit Lwt.t
      ; zenoh_sub : Zenoh.sub option
      }

    type session =
      { properties: properties
      ; ws : Path.t WsMap.t
      ; lastWsid: WsId.t
      ; subs : subscriber SubscriberMap.t
      ; evals : (eval_call * Zenoh.storage option) EvalMap.t
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
      ; zenoh_storage : Zenoh.storage option             
      }

    type t = state Guard.t

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
      let self = Guard.get admin in
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
      let self = Guard.get admin in
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

    let get_frontend_session self feid sid =
      match FrontendMap.find_opt feid self.frontends with
        | None -> Lwt.fail @@ YException (`InternalError (`Msg ("No frontend with id: "^(FeId.to_string feid))))
        | Some fe -> 
          (match SessionMap.find_opt sid fe.sessions with
          | None -> Lwt.fail @@ YException (`InternalError (`Msg ("No session with id: "^(SessionId.to_string sid))))
          | Some s -> Lwt.return (fe, s))

    let update_yaks_json_view prefix time kvs =
      let rec add_to_json (json:Yojson.Basic.json) (path:string list) (v:Yojson.Basic.json) =
        match json, path with
        | `Assoc l , [] -> `Assoc l
        | `Assoc l , p::[] -> `Assoc ((p, v)::l)
        | `Assoc l , p::tl -> (match List.find_opt (fun (s,_) -> s = p) l with
          | Some (_,j) -> let l' = List.filter (fun (s,_) -> s <> p) l in `Assoc ((p, add_to_json j tl v)::l')
          | None -> `Assoc ((p, add_to_json (`Assoc []) tl v)::l)
        )
        | _, _ -> failwith ("INTERNAL ERROR: Unexpected JSON type reconstructing value for "^prefix)
      in
      let selector = Selector.of_string @@ prefix^"/**" in
      let keys = KVMap.filter (fun path _ -> (not @@ (Path.to_string path = prefix)) && Selector.is_matching_path path selector) kvs
        |> KVMap.bindings
        |> List.map (fun (p,(v:TimedValue.t)) ->
            let subpath = Path.remove_prefix (String.length prefix +1) p |> Path.to_string in
            match Value.transcode v.value Value.JSON with
            | Ok js -> let json = Yojson.Basic.from_string @@ Value.to_string js in (String.split_on_char '/' subpath, json)
            | Error _ -> failwith ("INTERNAL ERROR: failed to transcode to JSON the value of  "^(Path.to_string p)))
      in
      let json = List.fold_left (fun json (path, v) -> add_to_json json path v)
       (`Assoc []) keys |> Yojson.Basic.to_string in
      kvmap_add (Path.of_string prefix) (Value.JSonValue json) time kvs

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
      Logs.debug (fun m -> m "[Yadm] add_backend : %s" id);
      Guard.guarded admin
      @@ fun self ->
      if BackendMap.mem BE.id self.backends then
        Guard.return_lwt (Lwt.fail @@ YException (`Forbidden (`Msg ("Already existing backend: "^id)))) self
      else
        let time = now self.hlc in
        let kvs = kvmap_add
          (Path.of_string @@ Printf.sprintf "%s/backend/%s/" self.admin_prefix id)
          (Value.PropertiesValue BE.properties) time self.kvs
          |> update_yaks_json_view self.admin_prefix time
        in
        Guard.return () { self with backends = BackendMap.add BE.id backend self.backends; kvs }

    let remove_backend admin beid = 
      let _ = ignore admin and _ = ignore beid in
      Lwt.fail @@ YException (`InternalError (`Msg ("remove_backend not yet implemented")))


    (**************************)
    (*   Storages management  *)
    (**************************)
    let find_compatible_backend backends properties =
      BackendMap.filter (fun beid be ->
        let module BE = (val be.beModule: Backend) in
        Logs.debug (fun m -> m "[Yadm]:    try Backend %s (%s)" (BeId.to_string beid) BE.to_string);
        Properties.not_conflicting properties BE.properties) backends
      |> BackendMap.choose_opt
    
    let create_storage admin ?beid stid properties time =
      (* get "selector" from properties *)
      let%lwt selector = match Properties.find_opt "selector" properties with
        | None -> Lwt.fail @@ YException (`InternalError (`Msg ("create_storage "^stid^" without 'selector' in properties")))
        | Some s -> (match Selector.of_string_opt s with 
          | None -> Lwt.fail @@ YException (`InternalError (`Msg ("create_storage "^stid^" with invalid 'selector': "^s)))
          | Some s' -> Lwt.return s')
      in
      let storageId = StorageId.of_string stid in
      Logs.debug (fun m -> m "[Yadm] create_storage %s on %s" stid (Selector.to_string selector));
      Guard.guarded admin
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
        Logs.debug (fun m -> m "[Yadm]: create_storage %s using Backend %s" stid (BE.to_string));
        let%lwt storage = BE.create_storage selector properties self.hlc in
        let%lwt zenoh_storage =  
          match self.zenoh with 
          | Some zenoh ->
            Storage.align storage zenoh selector >>
            lazy (ZUtils.store zenoh self.hlc selector (Storage.on_zenoh_write storage) (Storage.get storage)) >>=
            Lwt.return_some
          | None -> Lwt.return_none
        in
        let be' = { be with storages = StorageMap.add storageId (storage, zenoh_storage) be.storages } in
        let kvs = kvmap_add
          (Path.of_string @@ Printf.sprintf "%s/backend/%s/storage/%s" self.admin_prefix (BeId.to_string beid') stid)
          (Value.PropertiesValue properties) time self.kvs
          |> update_yaks_json_view self.admin_prefix time
        in
        Guard.return () { self with backends = BackendMap.add beid' be' self.backends; kvs }

    let remove_storage admin beid stid =
      let beid' = BeId.of_string beid in
      let stid' = StorageId.of_string stid in
      Logs.debug (fun m -> m "[Yadm] remove_storage %s/%s" beid stid);
      Guard.guarded admin
      @@ fun self ->
      let%lwt be = match BackendMap.find_opt beid' self.backends with
        | None -> Lwt.fail @@ YException (`InternalError (`Msg ("No backend with id: "^beid)))
        | Some be -> Lwt.return be
      in
      match StorageMap.find_opt stid' be.storages with
      | Some (storage, zenoh_storage) ->
        Storage.dispose storage >>= fun () ->
        let be' = { be with storages = StorageMap.remove stid' be.storages } in
        let time = now self.hlc in
        let kvs = KVMap.remove (Path.of_string @@ Printf.sprintf "%s/backend/%s/storage/%s" self.admin_prefix beid stid) self.kvs
          |> update_yaks_json_view self.admin_prefix time
        in
        let open Apero.Option.Infix in 
        let _ = self.zenoh 
        >>= fun zenoh -> 
          zenoh_storage >|= fun storage ->  ZUtils.unstore zenoh storage 
        in 
        Guard.return () { self with backends = BackendMap.add beid' be' self.backends; kvs }
      | None -> 
        Logs.debug (fun m -> m "[Yadm] storage %s/%s not found... ignore remove request" beid stid);
        Guard.return () self

    let get_matching_storages admin sel =
      (* @TODO: depending on quorum, use Storage.covers_fully *)
      let get_matching_storages backend =
        StorageMap.filter (fun _ (storage, _) -> Storage.covers_partially storage sel) backend.storages
        |> StorageMap.bindings |> List.map (fun (_,(s, _)) -> s)
      in
      let self = Guard.get admin in 
        BackendMap.fold (fun _ backend l -> (get_matching_storages backend)::l) self.backends [] |> List.concat


    (***************************)
    (*   Frontends management  *)
    (***************************)
    let add_frontend admin feid properties time =
      Logs.debug (fun m -> m "[Yadm] add_frontend %s" feid);
      Guard.guarded admin
      @@ fun self ->
      if FrontendMap.mem (FeId.of_string feid) self.frontends then
        Guard.return_lwt (Lwt.fail @@ YException (`InternalError (`Msg ("Already existing frontend with id: "^feid)))) self
      else
        let fe = { properties; sessions = SessionMap.empty } in
        let kvs = kvmap_add
          (Path.of_string @@ Printf.sprintf "%s/frontend/%s" self.admin_prefix feid)
          (Value.PropertiesValue properties) time self.kvs
          |> update_yaks_json_view self.admin_prefix time
        in
        Guard.return () { self with frontends = FrontendMap.add (FeId.of_string feid) fe self.frontends; kvs }

    let add_frontend_TMP admin feid properties =
      let self = Guard.get admin in
      add_frontend admin feid properties (now self.hlc)

    let remove_frontend admin feid = 
      Logs.debug (fun m -> m "[Yadm] remove_frontend %s" feid);
      Guard.guarded admin
      @@ fun self ->
      if not @@ FrontendMap.mem (FeId.of_string feid) self.frontends then (
        Logs.warn (fun m -> m "[Yadm] remove non-existing frontend: %s" feid);
        Guard.return () self)
      else
        let time = now self.hlc in
        let kvs = KVMap.remove (Path.of_string @@ Printf.sprintf "%s/frontend/%s" self.admin_prefix feid) self.kvs
          |> update_yaks_json_view self.admin_prefix time
        in
        Guard.return () { self with frontends = FrontendMap.remove (FeId.of_string feid) self.frontends; kvs }


    (****************************)
    (*   Workspaces management  *)
    (****************************)
    let add_workspace admin (clientid:ClientId.t) path =
      if Path.is_relative path then
        Lwt.fail @@ YException (`InternalError (`Msg ("Invalid workspace with non-absolute path: "^(Path.to_string path))))
      else
        begin
        Logs.debug (fun m -> m "[Yadm] %s: workspace %s" (ClientId.to_string clientid) (Path.to_string path));
        Guard.guarded admin
        @@ fun self ->
        let%lwt (fe, s) = get_frontend_session self clientid.feid clientid.sid in
        let wsid = WsId.add s.lastWsid WsId.one in
        let s' = { s with ws = WsMap.add wsid path s.ws; lastWsid = wsid } in
        let fe' = {fe with sessions = SessionMap.add clientid.sid s' fe.sessions} in
        let time = now self.hlc in
        let kvs = kvmap_add
          (Path.of_string @@ Printf.sprintf "%s/frontend/%s/session/%s/workspace/%s"
            self.admin_prefix (FeId.to_string clientid.feid) (SessionId.to_string clientid.sid) (WsId.to_string wsid))
          (Value.PropertiesValue (Properties.singleton "path" (Path.to_string path))) time self.kvs
          |> update_yaks_json_view self.admin_prefix time
        in
        Guard.return wsid { self with frontends = FrontendMap.add clientid.feid fe' self.frontends; kvs }
        end

    let get_workspace_path admin (clientid:ClientId.t) wsid =
      let self = Guard.get admin in 
      let%lwt (_, s) = get_frontend_session self clientid.feid clientid.sid in
      match WsMap.find_opt wsid s.ws with
      | None -> Lwt.fail @@ YException (`InternalError (`Msg ("No workspace with id: "^(WsId.to_string wsid))))
      | Some path -> Lwt.return path

    (*******************************)
    (*   Subscriptions management  *)
    (*******************************)
    let create_zenoh_subscriber zenoh_opt hlc selector is_push notify_call =
      match zenoh_opt with
      | Some zenoh -> ZUtils.subscribe zenoh hlc selector is_push notify_call >>= Lwt.return_some
      | None -> Lwt.return_none

    let remove_zenoh_subscriber zenoh subscriber =
      match subscriber.zenoh_sub with
      | Some sub -> ZUtils.unsubscribe zenoh sub
      | None -> Lwt.return_unit

     let remove_subscriber admin (clientid:ClientId.t) subid =
      Logs.debug (fun m -> m "[Yadm] %s: remove_subscriber %s" (ClientId.to_string clientid) (SubscriberId.to_string subid));
      Guard.guarded admin 
      @@ fun self ->
        let%lwt (fe, s) = get_frontend_session self clientid.feid clientid.sid in
        match SubscriberMap.find_opt subid s.subs with
        | Some subscriber -> 
          let _ = Option.map self.zenoh @@ fun z -> remove_zenoh_subscriber z subscriber in
          let s' = { s with subs = SubscriberMap.remove subid s.subs } in
          let fe' = { fe with sessions = SessionMap.add clientid.sid s' fe.sessions } in
          let time = now self.hlc in
          let kvs = KVMap.remove
            (Path.of_string @@ Printf.sprintf "%s/frontend/%s/session/%s/subscriber/%s"
              self.admin_prefix (FeId.to_string clientid.feid) (SessionId.to_string clientid.sid) (SubscriberId.to_string subid))
            self.kvs
            |> update_yaks_json_view self.admin_prefix time
          in
          Guard.return () { self with frontends = FrontendMap.add clientid.feid fe' self.frontends; kvs }
        | None ->
          Logs.warn (fun m -> m "[Yadm] %s: can't remove subscriber %s : not found" (ClientId.to_string clientid) (SubscriberId.to_string subid));
          Guard.return () self

    let create_subscriber admin (clientid:ClientId.t) selector is_push notifier =
      Logs.debug (fun m -> m "[Yadm] %s: create_subscriber %s" (ClientId.to_string clientid) (Selector.to_string selector));
      Guard.guarded admin 
      @@ fun self ->
        let%lwt (fe, s) = get_frontend_session self clientid.feid clientid.sid in
        let subid = SubscriberId.next_id () in
        let notify_call path changes =
          Logs.debug (fun m -> m "[Yadm] notify subscriber %s/%s for %d changes on %s" (ClientId.to_string clientid) (SubscriberId.to_string subid) (List.length changes) (Path.to_string path));
          notifier subid ~fallback:(remove_subscriber admin clientid) path changes 
        in
        let%lwt zenoh_sub = create_zenoh_subscriber self.zenoh self.hlc selector is_push notify_call in
        let sub = {selector; is_push; notify_call; zenoh_sub} in
        let s' = { s with subs = SubscriberMap.add subid sub s.subs } in
        let fe' = { fe with sessions = SessionMap.add clientid.sid s' fe.sessions } in
        let time = now self.hlc in
        let kvs = kvmap_add
          (Path.of_string @@ Printf.sprintf "%s/frontend/%s/session/%s/subscriber/%s"
            self.admin_prefix (FeId.to_string clientid.feid) (SessionId.to_string clientid.sid) (SubscriberId.to_string subid))
          (Value.PropertiesValue (Properties.singleton "selector" (Selector.to_string selector))) time self.kvs
          |> update_yaks_json_view self.admin_prefix time
        in
        Guard.return subid { self with frontends = FrontendMap.add clientid.feid fe' self.frontends; kvs }

    let notify_subscribers admin path changes =
      let iter_session _ _ s = SubscriberMap.iter
        (fun _ sub ->
          if Selector.is_matching_path path sub.selector then
            Lwt.ignore_result @@ sub.notify_call path changes
          else ()) s.subs
      in
      let iter_frontend feid fe = SessionMap.iter (fun sid session -> iter_session feid sid session) fe.sessions in
      let self = Guard.get admin in 
      FrontendMap.iter (fun feid fe -> iter_frontend feid fe) self.frontends

    (*****************************)
    (*     evals management      *)
    (*****************************)
    let remove_zenoh_eval zenoh eval =
      match eval with
      | (_, Some zstore) -> ZUtils.unstore zenoh zstore
      | _, _ -> Lwt.return_unit

    let remove_eval admin (clientid:ClientId.t) path =
      Logs.debug (fun m -> m "[Yadm] %s: remove_eval %s" (ClientId.to_string clientid) (Path.to_string path));
      Guard.guarded admin 
      @@ fun self ->
        let%lwt (fe, s) = get_frontend_session self clientid.feid clientid.sid in
        match EvalMap.find_opt path s.evals with
        | Some eval -> 
          let _ = Option.map self.zenoh @@ fun z -> remove_zenoh_eval z eval in
          let s' = { s with evals = EvalMap.remove path s.evals } in
          let fe' = { fe with sessions = SessionMap.add clientid.sid s' fe.sessions } in
          let time = now self.hlc in
          let kvs = KVMap.remove
            (Path.of_string @@ Printf.sprintf "%s/frontend/%s/session/%s/eval/%s"
              self.admin_prefix (FeId.to_string clientid.feid) (SessionId.to_string clientid.sid) (Path.to_string path))
            self.kvs
            |> update_yaks_json_view self.admin_prefix time
          in
          Guard.return () { self with frontends = FrontendMap.add clientid.feid fe' self.frontends; kvs }
        | None ->
          Logs.warn (fun m -> m "[Yadm] %s: can't remove eval %s : not found" (ClientId.to_string clientid) (Path.to_string path));
          Guard.return () self

    let call_evals admin (clientid:ClientId.t) multiplicity sel =
      Logs.debug (fun m -> m "[YAdm]: Call eval on %s for %s" (ClientId.to_string clientid) (Selector.to_string sel));
      let _ = ignore clientid in
      let add_eval p e m = if EvalMap.mem p m then EvalMap.add p (e::EvalMap.find p m) m else EvalMap.add p (e::[]) m in
      let rec invoke path eval_calls multiplicity = 
        let sel' = Selector.of_path ?predicate:(Selector.predicate sel) ?properties:(Selector.properties sel) ?fragment:(Selector.fragment sel) path in
        match multiplicity with
        | 0 -> []
        | _ -> ((List.hd eval_calls) sel')::(invoke path (List.tl eval_calls) (multiplicity-1))   (* TODO: call a random eval instead of the 1st in list *)
      in
      let self = Guard.get admin in
      FrontendMap.fold (fun _ fe m ->
        SessionMap.fold (fun _ s m ->
          EvalMap.fold (fun p (eval_call, _) m ->
            if Selector.is_matching_path p sel then
              add_eval p eval_call m
            else
              m
          ) s.evals m
        ) fe.sessions m
      ) self.frontends EvalMap.empty
      |> EvalMap.bindings
      |> List.map (fun (p,l) -> invoke p l (max multiplicity @@ List.length l) |> LwtM.flatten >>= fun l' -> Lwt.return (p,l'))
      |> LwtM.flatten

    let incoming_eval_data_handler path _ =
      Logs.warn (fun m -> m "[YAdm]: Received pushed data for eval %s - Ignore it!!" (Path.to_string path));
      Lwt.return_unit (* Eval should never get value "put" *)

    let zenoh_eval_prefix = "+"

    let incoming_eval_query_handler path hlc (eval_call:eval_call) zselector = 
      Logs.debug (fun m -> m "[YAdm]: Handling remote Zenoh query on eval '%s' for '%s'" (Path.to_string path) (Selector.to_string zselector));
      if Astring.is_prefix ~affix:zenoh_eval_prefix (Selector.path zselector) then
        let selector = Selector.of_string @@ Astring.with_range ~first:1 (Selector.to_string zselector) in
        let%lwt value = eval_call (Selector.with_path path selector) in
        let%lwt time = HLC.new_timestamp hlc in
        let (tv:TimedValue.t) =  {time; value} in
        Lwt.return [path, tv]
      else (
        Logs.err (fun m -> m "[YAdm]: Internal error the Zenoh resource name for eval doesn't start with + : %s" (Selector.to_string zselector));
        Lwt.return [])

    let create_zenoh_eval zenoh_opt hlc path eval_call =
      match zenoh_opt with
      (* NB:
          - Currently an eval is represented with a storage, once zenoh will support something like evals, we'll
            transition to that abstraction to avoid bu construction the progagation of spurious values.
          - The Zenoh storage selector for eval is the eval's path prefixed with '+'
      *)
      | Some zenoh ->
        let zenoh_eval_path = Selector.add_prefix ~prefix:(Path.of_string zenoh_eval_prefix) (Selector.of_path path) in
        ZUtils.store zenoh hlc zenoh_eval_path incoming_eval_data_handler (incoming_eval_query_handler path hlc eval_call) >>= Lwt.return_some
      | None -> Lwt.return_none

    let create_eval admin (clientid:ClientId.t) path (eval:eval_function) =
      Logs.debug (fun m -> m "[Yadm] %s: create_eval %s" (ClientId.to_string clientid) (Path.to_string path));
      let remove_eval_fallback path =
        remove_eval admin clientid path >>= fun _ ->
        Lwt.return @@ Value.StringValue
          (Printf.sprintf "Error calling eval for %s on %s: Eval implementer was removed"
          (Path.to_string path) (ClientId.to_string clientid))
      in
      let eval_call sel =
        Logs.debug (fun m -> m "[Yadm] call eval for %s on %s with %s" (Path.to_string path) (ClientId.to_string clientid) (Selector.to_string sel));
        eval path ~fallback:remove_eval_fallback sel
      in
      Guard.guarded admin 
      @@ fun self ->
        let%lwt (fe, s) = get_frontend_session self clientid.feid clientid.sid in
        let%lwt zenoh_eval = create_zenoh_eval self.zenoh self.hlc path eval_call in
        let s' = { s with evals = EvalMap.add path (eval_call, zenoh_eval) s.evals } in
        let fe' = {fe with sessions = SessionMap.add clientid.sid s' fe.sessions} in
        let time = now self.hlc in
        let kvs = kvmap_add
          (Path.of_string @@ Printf.sprintf "%s/frontend/%s/session/%s/eval/%s"
            self.admin_prefix (FeId.to_string clientid.feid) (SessionId.to_string clientid.sid) (Path.to_string path))
          empty_props_value time  self.kvs
          |> update_yaks_json_view self.admin_prefix time
        in
        Guard.return () { self with frontends = FrontendMap.add clientid.feid fe' self.frontends; kvs }

    (**************************)
    (*   Sessions management  *)
    (**************************)
    let login admin (clientid:ClientId.t) properties =
      Logs.debug (fun m -> m "[Yadm] %s: login" (ClientId.to_string clientid));
      Guard.guarded admin
      @@ fun self ->
      let feid = FeId.to_string clientid.feid in
      let sid = SessionId.to_string clientid.sid in
      match FrontendMap.find_opt clientid.feid self.frontends with
        | None -> Guard.return_lwt (Lwt.fail @@ YException (`InternalError (`Msg ("No frontend with id: "^feid)))) self
        | Some fe ->
          if SessionMap.mem clientid.sid fe.sessions then
            Guard.return_lwt (Lwt.fail @@ YException (`InternalError (`Msg ("Already existing session: "^sid)))) self
          else
            let s = { properties; ws=WsMap.empty; lastWsid=WsId.zero; subs=SubscriberMap.empty; evals=EvalMap.empty } in
            let fe' = {fe with sessions = SessionMap.add clientid.sid s fe.sessions} in
            let time = now self.hlc in
            let kvs = kvmap_add
              (Path.of_string @@ Printf.sprintf "%s/frontend/%s/session/%s" self.admin_prefix feid sid)
              (Value.PropertiesValue properties) time self.kvs
             |> update_yaks_json_view self.admin_prefix time
            in
            Guard.return () { self with frontends = FrontendMap.add clientid.feid fe' self.frontends; kvs }

    let cleanup_session zenoh_opt s =
      match zenoh_opt with
      | Some zenoh ->
        Lwt.join @@
          (SubscriberMap.fold (fun _ sub l -> (remove_zenoh_subscriber zenoh sub)::l ) s.subs []) @
          (EvalMap.fold (fun _ eval l -> (remove_zenoh_eval zenoh eval)::l) s.evals [])
      | None -> Lwt.return_unit

    let logout admin (clientid:ClientId.t) =
      Logs.debug (fun m -> m "[Yadm] %s: logout" (ClientId.to_string clientid));
      Guard.guarded admin
      @@ fun self ->
      match FrontendMap.find_opt clientid.feid self.frontends with
        | None -> Logs.warn (fun m -> m "[Yadm] logout from non-existing frontend: %s" (FeId.to_string clientid.feid));
          Guard.return () self
        | Some fe ->
          match SessionMap.find_opt clientid.sid fe.sessions with
          | None ->
            Logs.warn (fun m -> m "[Yadm] logout from non-existing session: %s" (ClientId.to_string clientid));
            Guard.return () self
          | Some s ->
            let () = Lwt.async @@ fun () -> cleanup_session self.zenoh s in
            let fe' = {fe with sessions = SessionMap.remove clientid.sid fe.sessions} in
            let spath = Path.of_string @@ Printf.sprintf "%s/frontend/%s/session/%s"
                self.admin_prefix (FeId.to_string clientid.feid) (SessionId.to_string clientid.sid)
            in
            let time = now self.hlc in
            let kvs = KVMap.filter (fun p _ -> not @@ Path.is_prefix ~affix:spath p) self.kvs
              |> update_yaks_json_view self.admin_prefix time
            in
            Guard.return () { self with frontends = FrontendMap.add clientid.feid fe' self.frontends; kvs }

    (*****************************)
    (* get/put/remove operations *)
    (*****************************)
    let get admin (clientid:ClientId.t) selector =
      let _ = ignore clientid in  (* will be used for access control*)
      Logs.debug (fun m -> m "[Yadm] %s: get %s" (ClientId.to_string clientid) (Selector.to_string selector));
      let self = Guard.get admin in 
      Lwt.return @@ match Selector.as_unique_path selector with 
      | Some path ->
        (match KVMap.find_opt path self.kvs with 
        | Some v -> [(path, v)]
        | None -> [])
      | None -> 
        self.kvs
          |> KVMap.filter (fun path _ -> Selector.is_matching_path path selector)
          |> KVMap.bindings

    let put admin (clientid:ClientId.t) path (tvalue:TimedValue.t) =
      let _ = ignore clientid in  (* will be used for access control*)
      Logs.debug (fun m -> m "[Yadm] %s: put %s" (ClientId.to_string clientid) (Path.to_string path));
      let time = tvalue.time in
      let%lwt properties = 
        let open Value in
        match transcode tvalue.value PROPERTIES with
        | Ok PropertiesValue p -> Lwt.return p
        | Ok _ -> Lwt.fail @@ YException (`UnsupportedTranscoding (`Msg "Transcoding to Properties didn't return a PropertiesValie"))
        | Error e -> Lwt.fail @@ YException e
      in
      let self = Guard.get admin in 
      if Astring.is_prefix ~affix:self.admin_prefix (Path.to_string path) then
        match String.split_on_char '/' @@ Astring.with_range ~first:(String.length self.admin_prefix+1) (Path.to_string path) with
        | ["frontend"; feid] -> add_frontend admin feid properties time
        | ["backend"; "auto"]  -> Lwt.fail @@ YException (`InvalidPath (`Msg ("Creation of backend/auto forbidden (reserved id)")))
        | ["backend"; beid]  -> add_backend admin beid properties time
        | ["backend"; "auto"; "storage"; stid] -> create_storage admin stid properties time
        | ["backend"; beid; "storage"; stid] -> create_storage admin ~beid stid properties time
        | _ -> Lwt.fail @@ YException (`InvalidPath (`Msg ("Invalid path on admin space: "^(Path.to_string path))))
      else
        Lwt.fail @@ YException (`InternalError (`Msg ("put on remote Yaks admin not yet implemented")))

    let remove admin (clientid:ClientId.t) path time =
      let _ = ignore clientid in  (* will be used for access control*)
      let _ = ignore time in (* TODO: removed map ?? *)
      Logs.debug (fun m -> m "[Yadm] %s: remove %s" (ClientId.to_string clientid) (Path.to_string path));
      let self = Guard.get admin in 
      if Astring.is_prefix ~affix:self.admin_prefix (Path.to_string path) then
        match String.split_on_char '/' @@ Astring.with_range ~first:(String.length self.admin_prefix+1) (Path.to_string path) with
        | ["frontend"; feid] -> remove_frontend admin feid
        | ["backend"; beid]  -> remove_backend admin beid
        | ["backend"; beid; "storage"; stid] -> remove_storage admin beid stid
        | _ -> Lwt.fail @@ YException (`InvalidPath (`Msg ("Invalid path on admin space: "^(Path.to_string path))))
      else
        Lwt.fail @@ YException (`InternalError (`Msg ("put on remote Yaks admin not yet implemented")))


    let incoming_admin_storage_data_handler admin (path:Path.t) (changes:change list) =
      Logs.debug (fun m -> m "[YAdm]: Received remote changes for key %s" (Path.to_string path));
        let self = Guard.get admin in
        let check_time_validity time =
          match%lwt HLC.update_with_timestamp time self.hlc with
          | Ok () -> Lwt.return_true
          | Error e -> Logs.warn (fun m -> m "[Yadm]: Incoming change from Zenoh for %s refused: timestamp differs too much from local clock: %s" (Path.to_string path) (Apero.show_error e)); Lwt.return_false
        in
        Lwt_list.iter_s (function
          | Put(tv)      -> if%lwt check_time_validity tv.time then put admin local_client path tv
          | Remove(time) -> if%lwt check_time_validity time then remove admin local_client path time
          | Update(_)    -> Logs.warn (fun m -> m "[YAdm]: Received update for %s : only put or remove are supported by Admin space" (Path.to_string path)); Lwt.return_unit
        ) changes

    let incoming_admin_query_handler admin selector =
      (* NOTE: check if selector starts with "/@/"" to not answer to "/**"" queries *)
      if Astring.is_prefix ~affix:"/@/" (Selector.path selector) then
        begin
          get admin local_client selector
        end
      else
        Lwt.return []

    let make yid hlc zenoh =
      let admin_prefix = "/@/"^(Uuid.to_string yid) in
      Logs.debug (fun m -> m "Create Yaks %s admin space\n" admin_prefix);
      let time = now hlc in
      let kvs = KVMap.empty
        |> kvmap_add (Path.of_string admin_prefix) empty_props_value time
        |> fun kvs -> match zenoh with
          | Some z -> kvmap_add
            (Path.of_string @@ admin_prefix^"/transport/zenoh")
            (Value.PropertiesValue (Zenoh.info z)) time kvs
          | None -> kvs
      in
      let admin =  Guard.create
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
        let selector = Selector.of_string @@ admin_prefix^"/**" in
        let%lwt s = ZUtils.store z hlc selector (incoming_admin_storage_data_handler admin) (incoming_admin_query_handler admin) in
        Lwt.return @@ Some s
      | None -> Lwt.return None 
      in 
      let _  = 
        Guard.guarded admin 
        @@ fun self -> 
          let%lwt zenoh_storage = zenoh_storage_lwt in 
          Guard.return () {self with zenoh_storage}  
      in admin

  end

