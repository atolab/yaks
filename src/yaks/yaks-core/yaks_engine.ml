open Apero
open Apero.LwtM.InfixM
open Yaks_common_errors
open Yaks_types
open Yaks_core_types
open Yaks_be
open Yaks_storage


module Engine = struct 

  module type S = sig 
    type t 

    val make : ?id:string -> Zenoh.t option -> t

    val login : t -> ClientId.t -> properties -> unit Lwt.t
    val logout : t -> ClientId.t -> unit Lwt.t   

    val add_workspace : t -> ClientId.t -> Path.t -> WsId.t Lwt.t

    val get : t -> ClientId.t -> ?quorum:int -> ?workspace:WsId.t -> Selector.t -> (Path.t * Value.t) list  Lwt.t
    val put : t -> ClientId.t -> ?quorum:int -> ?workspace:WsId.t -> Path.t -> Value.t -> unit Lwt.t
    val update : t -> ClientId.t -> ?quorum:int -> ?workspace:WsId.t -> Path.t -> Value.t -> unit Lwt.t
    val remove : t -> ClientId.t -> ?quorum:int -> ?workspace:WsId.t -> Path.t -> unit Lwt.t

    val subscribe : t -> ClientId.t -> ?workspace:WsId.t -> Selector.t -> bool -> notify_subscriber -> SubscriberId.t Lwt.t  
    val unsubscribe : t -> ClientId.t -> SubscriberId.t -> unit Lwt.t  

    val register_eval : t -> ClientId.t -> ?workspace:WsId.t -> Path.t -> eval_function -> unit Lwt.t
    val unregister_eval : t -> ClientId.t -> ?workspace:WsId.t -> Path.t -> unit Lwt.t
    val eval : t -> ClientId.t -> ?multiplicity:int -> ?workspace:WsId.t -> Selector.t -> (Path.t * Value.t) list  Lwt.t

    (* TODO: Temporary operations that should be replaced by put/get/remove usage *)
    val add_backend_TMP : t -> (module Backend) -> unit Lwt.t
    val add_frontend_TMP : t -> string -> properties -> unit Lwt.t
  end
    module YAdminSpace = Yaks_admin_space.AdminSpace

    module KVListMap = Map.Make(Path)

    module ZUtils = Yaks_zenoh_utils

    type state =
      { yid: Yid.t
      ; admin : YAdminSpace.t
      ; zenoh : Zenoh.t option
      ; hlc : HLC.t }

    type t = state Guard.t

    let make ?id zenoh =
      let yid =
        let (log,id') =
          match id with
          | None -> "random id", (Uuid.to_string @@ Uuid.make ())
          | Some id -> "specified id", id
        in
        Logs.debug (fun m -> m "Starting Yaks with %s: %s" log id');
        id'
      in
      let hlc = HLC.create (Uuid.make_from_alias yid) in
      Guard.create 
        { yid
        ; admin = YAdminSpace.make yid hlc zenoh 
        ; zenoh
        ; hlc }
    
    let to_absolute_path engine clientid ?workspace path =
      if Path.is_relative path then
        match workspace with 
        | None -> Lwt.fail @@ YException (`InternalError (`Msg ("Relative path specified without workspace: "^(Path.to_string path))))
        | Some wsid -> 
          let self = Guard.get engine in 
          let%lwt base = YAdminSpace.get_workspace_path self.admin clientid wsid in
          Lwt.return @@ Path.add_prefix ~prefix:base path
      else Lwt.return path

    let to_absolute_selector engine clientid ?workspace selector =
      if Selector.is_relative selector then
        match workspace with 
        | None -> Lwt.fail @@ YException (`InternalError (`Msg ("Relative selector specified without workspace: "^(Selector.path selector))))
        | Some wsid -> 
          let self = Guard.get engine in 
          let%lwt base = YAdminSpace.get_workspace_path self.admin clientid wsid in
          Lwt.return @@ Selector.add_prefix ~prefix:base selector
      else Lwt.return selector

    let map_of_results (results:(Path.t*'a) list)= List.fold_left (
      fun m (p,v) -> match KVListMap.find_opt p m with
        | Some l -> KVListMap.add p (v::l) m
        | None -> KVListMap.add p (v::[]) m)
      KVListMap.empty results


    (********************)
    (*  login / logout  *)
    (********************)
    let login engine clientid properties =
      let self = Guard.get engine in 
      YAdminSpace.login self.admin clientid properties

    let logout engine clientid =
      let self = Guard.get engine in 
      YAdminSpace.logout self.admin clientid

    let add_workspace engine clientid path =
      let self = Guard.get engine in 
      YAdminSpace.add_workspace self.admin clientid path

    (****************************)
    (*  Subscribers management  *)
    (****************************)
    let subscribe engine clientid ?workspace selector (is_push: bool) (notifier:notify_subscriber) =
      (* TODO: access control *)
      let%lwt sel = to_absolute_selector engine clientid ?workspace selector in
      Logs.debug (fun m -> m "[Yeng] %s: subscribe %s" (ClientId.to_string clientid) (Selector.to_string sel));
      let self = Guard.get engine in 
      YAdminSpace.create_subscriber self.admin clientid sel is_push notifier

    let unsubscribe engine clientid sid =
      Logs.debug (fun m -> m "[Yeng]: %s unsubscribe %s" (ClientId.to_string clientid) (SubscriberId.to_string sid));
      let self = Guard.get engine in 
      YAdminSpace.remove_subscriber self.admin clientid sid


    (****************************)
    (*     Eval management      *)
    (****************************)
    let register_eval engine clientid ?workspace path eval =
      (* TODO: access control *)
      let%lwt pat = to_absolute_path engine clientid ?workspace path in
      Logs.debug (fun m -> m "[Yeng]: register_eval %s" (Path.to_string pat));
      let self = Guard.get engine in 
      YAdminSpace.create_eval self.admin clientid pat eval

    let unregister_eval engine clientid ?workspace path =
      (* TODO: access control *)
      let%lwt pat = to_absolute_path engine clientid ?workspace path in
      Logs.debug (fun m -> m "[Yeng]: unregister_eval %s" (Path.to_string pat));
      let self = Guard.get engine in 
      YAdminSpace.remove_eval self.admin clientid pat

    let apply_multiplicity multiplicity kvlmap =
      (* TODO: implement multiplicity *)
      let _ = ignore multiplicity in
      KVListMap.fold (fun p vl res ->
        Logs.debug (fun m -> m "[Yeng]: apply multiplicity %d on %d received values for %s" multiplicity (List.length vl) (Path.to_string p));
        let v = List.hd vl in
        (p,v)::res)
      kvlmap []

    let eval engine client ?(multiplicity=1) ?workspace sel =
      (* TODO: access control *)
      (* TODO: transport with multiplicity *)
      let _ = ignore multiplicity in
      let%lwt sel = to_absolute_selector engine client ?workspace sel in
      Logs.debug (fun m -> m "[Yeng]: eval %s" (Selector.to_string sel));
      let self = Guard.get engine in 
      let local_evals = YAdminSpace.call_evals self.admin client 1 sel
        >|= List.fold_left (fun l (p, vl) -> (List.map (fun v -> p,v) vl)::l) []
        >|= List.concat
      in
      let remote_evals = match self.zenoh with
            | Some zenoh ->
              (* NB:
                  - Currently an eval is represented with a storage, once zenoh will support something like evals, we'll
                    transition to that abstraction to avoid bu construction the progagation of spurious values.
                  - The Zenoh storage selector for eval is the eval's path prefixed with '+'
              *)
              let sel' = Selector.add_prefix ~prefix:(Path.of_string "+") sel in
              ZUtils.query zenoh self.hlc sel' >|= List.map (fun ((p,tv):(Path.t*TimedValue.t)) -> (p,tv.value))
            | None -> Lwt.return []
      in
      LwtM.flatten [local_evals; remote_evals] >|= List.concat
      >|= map_of_results >|= apply_multiplicity multiplicity

    (*****************************)
    (*   Key/Value operations    *)
    (*****************************)
    let apply_quorum quorum kvlmap =
      let _ = ignore quorum in
      KVListMap.fold (fun p tvl res ->
        (* @TODO: for each path return appropriate value depending time and quorum *)
        Logs.debug (fun m -> m "[Yeng]: apply quorum %d on %d received values for %s" quorum (List.length tvl) (Path.to_string p));
        let tv:TimedValue.t = List.hd tvl in
        Logs.debug (fun m -> m "[Yeng]:   -> for %s choosed value with timestamp %a" (Path.to_string p) HLC.Timestamp.pp tv.time);
        (p,tv.value)::res)
      kvlmap []

    let get engine client ?(quorum=0) ?workspace sel =
      (* TODO: access control *)
      (* TODO: transport with quorum *)
      let%lwt sel = to_absolute_selector engine client ?workspace sel in
      (* @TODO: following is a temporary fix to correctly answer GET /@/** 
         This needs to be replaced by a real admin Storage and 
         YAdminSpace.get_storages_for_selector returning this Storage if selector intersect it
      *)
      let admin_results =
        let self = Guard.get engine in 
        if Astring.is_prefix ~affix:"/@/*" @@ Selector.path sel then
          YAdminSpace.get self.admin client sel
        else
          match YAdminSpace.covers_selector self.admin sel with
          | Some sel -> YAdminSpace.get self.admin client sel
          | None -> Lwt.return []
      in
      let storages_results =
        Logs.debug (fun m -> m "[Yeng]: get %s => query storages" (Selector.to_string sel));
        let self = Guard.get engine in 
        let storages = YAdminSpace.get_matching_storages self.admin sel in
        let fully_covering_storages = storages |> List.filter (fun s -> Storage.covers_fully s sel) in
        match fully_covering_storages with
        | s::_ -> (* Get results from the 1st fully covering storage *)
          Logs.debug (fun m -> m "[Yeng]: get %s => Local storage %s fully covers. Get only from it." (Selector.to_string sel) (Storage.to_string s) );
          Storage.get s sel
        | [] -> (* get from local + remote storages *)
          Logs.debug (fun m -> m "[Yeng]: get %s => get local + remote" (Selector.to_string sel));
          let local_get = storages |> List.map (fun store -> Storage.get store sel) |> Apero.LwtM.flatten >|= List.concat in
          let remote_get = (match self.zenoh with
            | Some zenoh -> ZUtils.query zenoh self.hlc sel
            | None -> Lwt.return [])
            >>= Lwt_list.filter_p (fun (p, (tv:TimedValue.t)) ->
              (* update HLC for each received TimedValue *)
              match%lwt HLC.update_with_timestamp tv.time self.hlc with
              | Ok () -> Lwt.return_true
              | Error e ->
                Logs.warn (fun m -> m "[Yeng]: get %s => received value key %s refused: timestamp differs too much from local clock: %s"
                  (Selector.to_string sel) (Path.to_string p) (Apero.show_error e));
                Lwt.return_false
              ) 
          in
          Lwt.join [(local_get >>= fun _ -> Lwt.return_unit); (remote_get >>= fun _ -> Lwt.return_unit)] >>= fun () ->
          remote_get >>= fun rd ->
          local_get >>= fun ld ->
          Logs.debug (fun m -> m "[Yeng]: get %s => found %d values locally and %d values remotely" (Selector.to_string sel) (List.length ld) (List.length rd));
          Lwt.return @@ List.append rd ld
      in
      LwtM.flatten [admin_results; storages_results] >|= List.concat
        >|= map_of_results >|= apply_quorum quorum


    let put engine client ?quorum ?workspace path value =
      (* TODO: access control *)
      (* TODO: transport with quorum *)
      let _ = ignore quorum in
      let%lwt path = to_absolute_path engine client ?workspace path in
      let self = Guard.get engine in
      
      let%lwt time = HLC.new_timestamp self.hlc in
      let tv: TimedValue.t = { time; value } in
      Logs.debug (fun m -> m "[Yeng]: put %s with timestamp %a" (Path.to_string path) HLC.Timestamp.pp time);
      match YAdminSpace.covers_path self.admin path with
      | Some path -> YAdminSpace.put self.admin client path tv
      | None ->
        YAdminSpace.notify_subscribers self.admin path [Put tv];
        Lwt.async begin fun () ->
          YAdminSpace.get_matching_storages self.admin (Selector.of_path path) |>
          Lwt_list.iter_p (fun store -> Storage.put store path tv)
        end;
        if Option.is_some self.zenoh then Lwt.async (fun () -> ZUtils.send_put path tv (Option.get self.zenoh));
        Lwt.return_unit

    let update engine client ?quorum ?workspace path value =
      (* TODO: access control *)
      (* TODO: transport with quorum *)
      let _ = ignore quorum in
      let%lwt path = to_absolute_path engine client ?workspace path in
      let self = Guard.get engine in 
      let%lwt time = HLC.new_timestamp self.hlc in
      let tv: TimedValue.t = { time; value } in
      Logs.debug (fun m -> m "[Yeng]: update %s with timestamp %a" (Path.to_string path) HLC.Timestamp.pp time);
      match YAdminSpace.covers_path self.admin path with
      | Some path ->
        Logs.err (fun m -> m "[Yeng]: update %s : not allowaed in admin space" (Path.to_string path));
        Lwt.return_unit
      | None ->
        YAdminSpace.notify_subscribers self.admin path [Update tv];
        Lwt.async begin fun () ->
          YAdminSpace.get_matching_storages self.admin (Selector.of_path path) |>
          Lwt_list.iter_p (fun store -> Storage.update store path tv)
        end;
        if Option.is_some self.zenoh then Lwt.async (fun () -> ZUtils.send_update path tv (Option.get self.zenoh));
        Lwt.return_unit

    let remove engine client ?quorum ?workspace path =
      (* TODO: access control *)
      (* TODO: transport with quorum *)
      let _ = ignore quorum in
      let%lwt path = to_absolute_path engine client ?workspace path in
      let self = Guard.get engine in       
      let%lwt time = HLC.new_timestamp self.hlc in
      Logs.debug (fun m -> m "[Yeng]: remove %s with timestamp %a" (Path.to_string path) HLC.Timestamp.pp time);
      match YAdminSpace.covers_path self.admin path with
      | Some path -> YAdminSpace.remove self.admin client path time
      | None ->
        YAdminSpace.notify_subscribers self.admin path [Remove time];
        Lwt.async begin fun () ->
          YAdminSpace.get_matching_storages self.admin (Selector.of_path path) |>
          Lwt_list.iter_p (fun store -> Storage.remove store path time)
        end;
        if Option.is_some self.zenoh then Lwt.async (fun () -> ZUtils.send_remove path time (Option.get self.zenoh));
        Lwt.return_unit

    let add_backend_TMP engine be = 
      let self = Guard.get engine in 
      YAdminSpace.add_backend_TMP self.admin be

    let add_frontend_TMP engine id properties =
      let self = Guard.get engine in  
      YAdminSpace.add_frontend_TMP self.admin id properties
 
  end

