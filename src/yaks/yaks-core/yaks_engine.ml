open Apero
open Yaks_common_errors
open Yaks_types
open Yaks_core_types
open Yaks_be
open Yaks_storage
open Apero.LwtM.InfixM


module Engine = struct 

  module type S = sig 
    type t 

    val make : Zenoh.t option -> t

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


  module Make (MVar: Apero.MVar) = struct

    module YAdminSpace = Yaks_admin_space.AdminSpace.Make(MVar)

    module KVListMap = Map.Make(Path)

    type state =
      { admin : YAdminSpace.t 
      ; zenoh : Zenoh.t option }

    type t = state MVar.t

    let id = my_yid

    let make zenoh =
      let _ = Logs_lwt.debug (fun m -> m "Creating Engine\n") in 
      MVar.create 
        { admin = YAdminSpace.make zenoh 
        ; zenoh }
    
    let to_absolute_path engine clientid ?workspace path =
      if Path.is_relative path then
        match workspace with 
        | None -> Lwt.fail @@ YException (`InternalError (`Msg ("Relative path specified without workspace: "^(Path.to_string path))))
        | Some wsid -> 
          let%lwt base = MVar.read engine >>= fun self -> YAdminSpace.get_workspace_path self.admin clientid wsid in
          Lwt.return @@ Path.add_prefix ~prefix:base path
      else Lwt.return path

    let to_absolute_selector engine clientid ?workspace selector =
      if Selector.is_relative selector then
        match workspace with 
        | None -> Lwt.fail @@ YException (`InternalError (`Msg ("Relative selector specified without workspace: "^(Selector.path selector))))
        | Some wsid -> 
          let%lwt base = MVar.read engine >>= fun self -> YAdminSpace.get_workspace_path self.admin clientid wsid in
          Lwt.return @@ Selector.add_prefix ~prefix:base selector
      else Lwt.return selector


    (*****************************)
    (* Query on Zenoh management *)
    (*****************************)
    let map_of_results (results:(Path.t*'a) list)= List.fold_left (
      fun m (p,v) -> match KVListMap.find_opt p m with
        | Some l -> KVListMap.add p (v::l) m
        | None -> KVListMap.add p (v::[]) m)
      KVListMap.empty results

     let remote_query_handler promise mlist decoder sample =
      MVar.guarded mlist
      @@ fun xs ->
        match sample with
        | Zenoh.StorageData {stoid; rsn=_; resname; data} ->
          (match
            let _ = Logs.debug (fun m -> m ">>> Query Handler Received data for key: %s" resname) in
            let store_id = IOBuf.hexdump ~separator:":" stoid in
            let _ = Logs.debug (fun m -> m ">>> Query Handler Received data for key: %s from storage %s" resname store_id) in
            match decoder data with
            | Ok (value, _) ->
              let _ = Logs.debug (fun m -> m ">>> Query Handler parsed data for key: %s" resname) in
              Ok(store_id, Path.of_string(resname), value)
            | Error e ->
              let _ = Logs.err (fun m -> m ">>> Query Handler failed to parse data for key %s : %s" resname (Atypes.show_error e)) in
              Error e
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


    let issue_remote_query zenoh selector decoder =
      let path = Selector.path selector in
      let p,r = Lwt.wait () in
      let mlist = MVar.create [] in
      let query =  (Option.get_or_default (Selector.predicate selector) "") in
      let%lwt () = Zenoh.query path query (remote_query_handler r mlist decoder) zenoh in
      let open Lwt.Infix in
      p >>= Lwt_list.map_p (fun (_,p,v) -> Lwt.return (p, v))     (* Note:  Zenoh StorageIds is ignored (no need) *)



    (********************)
    (*  login / logout  *)
    (********************)
    let login engine clientid properties =
      MVar.read engine >>= fun self -> YAdminSpace.login self.admin clientid properties

    let logout engine clientid =
      MVar.read engine >>= fun self -> YAdminSpace.logout self.admin clientid

    let add_workspace engine clientid path =
      MVar.read engine >>= fun self -> YAdminSpace.add_workspace self.admin clientid path

    (****************************)
    (*  Subscribers management  *)
    (****************************)
    let subscribe engine clientid ?workspace selector (is_push: bool) (notifier:notify_subscriber) =
      (* TODO: access control *)
      let%lwt sel = to_absolute_selector engine clientid ?workspace selector in
      let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng] %s: subscribe %s" (ClientId.to_string clientid) (Selector.to_string sel)) in
      MVar.read engine >>= fun self -> YAdminSpace.create_subscriber self.admin clientid sel is_push notifier

    let unsubscribe engine clientid sid =
      let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng]: %s unsubscribe %s" (ClientId.to_string clientid) (SubscriberId.to_string sid)) in
      MVar.read engine >>= fun self -> YAdminSpace.remove_subscriber self.admin clientid sid


    (****************************)
    (*     Eval management      *)
    (****************************)
    let register_eval engine clientid ?workspace path eval =
      (* TODO: access control *)
      let%lwt pat = to_absolute_path engine clientid ?workspace path in
      let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng]: register_eval %s" (Path.to_string pat)) in
      MVar.read engine >>= fun self -> YAdminSpace.create_eval self.admin clientid pat eval

    let unregister_eval engine clientid ?workspace path =
      (* TODO: access control *)
      let%lwt pat = to_absolute_path engine clientid ?workspace path in
      let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng]: unregister_eval %s" (Path.to_string pat)) in
      MVar.read engine >>= fun self -> YAdminSpace.remove_eval self.admin clientid pat

    let apply_multiplicity multiplicity kvlmap =
      (* TODO: implement multiplicity *)
      let _ = ignore multiplicity in
      KVListMap.fold (fun p vl res ->
        let _ = Logs_lwt.debug (fun m -> m "[Yeng]: apply multiplicity %d on %d received values for %s" multiplicity (List.length vl) (Path.to_string p)) in
        let v = List.hd vl in
        (p,v)::res)
      kvlmap []

    let eval engine client ?(multiplicity=1) ?workspace sel =
      (* TODO: access control *)
      (* TODO: transport with multiplicity *)
      let _ = ignore multiplicity in
      let%lwt sel = to_absolute_selector engine client ?workspace sel in
      let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng]: eval %s" (Selector.to_string sel)) in
      MVar.read engine >>= fun self ->
      let local_evals = YAdminSpace.call_evals self.admin client 1 sel
        >|= List.fold_left (fun l (p, vl) -> (List.map (fun v -> p,v) vl)::l) []
        >|= List.concat
      in
      let remote_evals = match self.zenoh with
            | Some zenoh ->
              (* NB:
                  - Currently an eval is represented with a storage, once zenoh will support something like evals, we'll
                    transition to that abstraction to avoid bu construction the progagation of spurious values.
                  - The Zenoh storage selector for eval is the eval's path prefixed with '/+'
              *)
              let sel' = Selector.add_prefix ~prefix:(Path.of_string "/+") sel in
              issue_remote_query zenoh sel' Yaks_fe_sock_codec.decode_value
            | None -> Lwt.return []
      in
      LwtM.flatten [local_evals; remote_evals] >|= List.concat
      >|= map_of_results >|= apply_multiplicity multiplicity


    (*****************************)
    (*   Key/Value operations    *)
    (*****************************)
    let admin_prefix_id = "/@/"^(Yid.to_string id)
    let admin_prefix_local = "/@/local"

    let check_if_admin_selector sel =
      let pat = Selector.path sel in
      if Astring.is_prefix ~affix:admin_prefix_local pat then
        (* if path starts with /@/local, convert to /@/my_yid *)
        Astring.with_range ~first:(String.length admin_prefix_local) pat
        |> Astring.append admin_prefix_id
        |> Selector.of_string
        |> Option.return
      else if Astring.is_prefix ~affix:admin_prefix_id pat then
        (* if path starts with /@/my_yid *)
        Some sel
      else
        None

    let check_if_admin_path path =
      let pat = Path.to_string path in
      if Astring.is_prefix ~affix:admin_prefix_local pat then
        (* if path starts with /@/local, convert to /@/my_yid *)
        Astring.with_range ~first:(String.length admin_prefix_local) pat
        |> Astring.append admin_prefix_id
        |> Path.of_string
        |> Option.return
      else if Astring.is_prefix ~affix:admin_prefix_id pat then
        (* if path starts with /@/my_yid *)
        Some path
      else
        None

    let apply_quorum quorum kvlmap =
      let _ = ignore quorum in
      KVListMap.fold (fun p tvl res ->
        (* @TODO: for each path return appropriate value depending time and quorum *)
        let _ = Logs_lwt.debug (fun m -> m "[Yeng]: apply quorum %d on %d received values for %s" quorum (List.length tvl) (Path.to_string p)) in
        let tv = List.hd tvl in
        let _ = Logs_lwt.debug (fun m -> m "[Yeng]:   -> for %s choosed value with timestamp %a" (Path.to_string p) HLC.Timestamp.pp tv.time) in
        (p,tv.value)::res)
      kvlmap []

    let get engine client ?(quorum=1) ?workspace sel =
      (* TODO: access control *)
      (* TODO: transport with quorum *)
      let%lwt sel = to_absolute_selector engine client ?workspace sel in
      (* @TODO: following is a temporary fix to correctly answer GET /@/** 
         This needs to be replaced by a real admin Storage and 
         YAdminSpace.get_storages_for_selector returning this Storage if selector intersect it
      *)
      let admin_results = if Astring.is_prefix ~affix:"/@/*" @@ Selector.path sel then
        MVar.read engine >>= fun self -> YAdminSpace.get self.admin client sel
        else
          match check_if_admin_selector sel with
          | Some sel -> MVar.read engine >>= fun self -> YAdminSpace.get self.admin client sel
          | None -> Lwt.return []
      in
      let storages_results =
        let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng]: get %s => query storages" (Selector.to_string sel)) in
        MVar.read engine >>= fun self ->
        let%lwt storages = YAdminSpace.get_storages_for_selector self.admin sel in
        match storages with
        | [] ->
          let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng]: No storage Matches, going remote...") in
          (match self.zenoh with
          | Some zenoh -> issue_remote_query zenoh sel decode_timed_value
          | None -> Lwt.return [])
        | _ ->
          let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng]: Some storage Matches, going local+remote") in
          let covers: bool =  storages
            |> List.map (fun s -> Storage.is_covering_selector s sel)
            |> List.fold_left (fun a b -> a || b) false
          in
          let local_get = List.map (fun store -> Storage.get store sel) storages
              |> Apero.LwtM.flatten
              >|= List.concat
          in 
          let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng]: Some storage Covers: %b" covers) in
          if covers then local_get
          else 
            match self.zenoh with
            | Some zenoh -> 
              let rdata = issue_remote_query zenoh sel decode_timed_value in
              let ldata = local_get in 
              Lwt.join [(rdata >>= fun _ -> Lwt.return_unit); (ldata >>= fun _ -> Lwt.return_unit)] 
              >>= fun () ->
                rdata >>= fun rd ->
                  ldata >>= fun ld -> 
                    let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng]: Local get returned %d values and remote get %d results" (List.length ld) (List.length rd)) in
                    Lwt.return @@ List.append rd ld
            | None -> local_get
        in
        LwtM.flatten [admin_results; storages_results] >|= List.concat
        >|= map_of_results >|= apply_quorum quorum


    let distribute_update path value zenoh =
      let res = Path.to_string path in
      let buf = IOBuf.create ~grow:8192 8192 in 
      match (encode_timed_value value buf) with 
      | Ok buf -> 
        let buf' = IOBuf.flip buf in 
        Zenoh.write buf' res zenoh 
      | Error e -> Lwt.fail @@ Exception e 

    let put engine client ?quorum ?workspace path value =
      (* TODO: access control *)
      (* TODO: transport with quorum *)
      let _ = ignore quorum in
      let%lwt time = HLC.update_with_clock () in
      let tv = { time; value } in
      let%lwt path = to_absolute_path engine client ?workspace path in
      let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng]: put %s with timestamp %a" (Path.to_string path) HLC.Timestamp.pp time) in
      MVar.read engine 
      >>= fun self ->      
      match check_if_admin_path path with
      | Some path -> MVar.read engine >>= fun self -> YAdminSpace.put self.admin client path tv
      | None ->
        let _ = YAdminSpace.notify_subscribers self.admin path value in
        let _ = YAdminSpace.get_storages_for_path self.admin path
        >>= Lwt_list.iter_p (fun store -> Storage.put store path tv) in 
        let open Apero.Option.Infix in 
        let _ = self.zenoh >|= fun zenoh -> (distribute_update path tv zenoh) in
        Lwt.return_unit

    let update engine client ?quorum ?workspace path value =
      (* TODO: access control *)
      (* TODO: transport with quorum *)
      let _ = ignore quorum in
      let%lwt time = HLC.update_with_clock () in
      let tv = { time; value } in
      let%lwt path = to_absolute_path engine client ?workspace path in
      let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng]: update %s with timestamp %a" (Path.to_string path) HLC.Timestamp.pp time) in
      MVar.read engine 
      >>= fun self ->
      let _ = Lwt.return @@ YAdminSpace.notify_subscribers self.admin path value in
      YAdminSpace.get_storages_for_path self.admin path
      >>= Lwt_list.iter_p (fun store -> Storage.update store path tv)

    let remove engine client ?quorum ?workspace path =
      (* TODO: access control *)
      (* TODO: transport with quorum *)
      let _ = ignore quorum in
      let%lwt path = to_absolute_path engine client ?workspace path in
      MVar.read engine 
      >>= fun self ->      
      match check_if_admin_path path with
      | Some path -> MVar.read engine >>= fun self -> YAdminSpace.remove self.admin client path
      | None ->
        let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng]: remove %s" (Path.to_string path)) in
        YAdminSpace.get_storages_for_path self.admin path
        >>= Lwt_list.iter_p (fun store -> Storage.remove store path)


    let add_backend_TMP engine be = 
      MVar.read engine >>= fun self -> YAdminSpace.add_backend_TMP self.admin be

    let add_frontend_TMP engine id properties =
      MVar.read engine >>= fun self -> YAdminSpace.add_frontend_TMP self.admin id properties
 
  end

end
