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

    val make : unit -> t 

    val get : t -> ClientId.t -> ?quorum:int -> ?workspace:WsId.t -> Selector.t -> (Path.t * Value.t) list  Lwt.t
    val put : t -> ClientId.t -> ?quorum:int -> ?workspace:WsId.t -> Path.t -> Value.t -> unit Lwt.t
    val update : t -> ClientId.t -> ?quorum:int -> ?workspace:WsId.t -> Path.t -> Value.t -> unit Lwt.t
    val remove : t -> ClientId.t -> ?quorum:int -> ?workspace:WsId.t -> Path.t -> unit Lwt.t

    val subscribe : t -> ClientId.t -> ?workspace:WsId.t -> Selector.t -> bool -> notify_subscriber -> SubscriberId.t Lwt.t  
    val unsubscribe : t -> ClientId.t -> SubscriberId.t -> unit Lwt.t  

    val eval : t -> ClientId.t -> ?workspace:WsId.t -> Path.t -> get_on_eval -> unit Lwt.t

    (* TODO: Temporary operations that should be replaced by put/get/remove usage *)
    val add_backend_TMP : t -> string -> (module Backend) -> unit Lwt.t
    val add_frontend_TMP : t -> string -> properties -> unit Lwt.t
  end

  module Make (MVar: Apero.MVar) = struct

    module YAdminSpace = Yaks_admin_space.AdminSpace.Make(MVar)

    module EvalMap = Map.Make (Path)

    type state =
      { admin : YAdminSpace.t 
      ; evals : get_on_eval EvalMap.t
      }

    type t = state MVar.t

    let make () =
      let _ = Logs_lwt.debug (fun m -> m "Creating Engine\n") in 
      MVar.create 
        { admin = YAdminSpace.make ()
        ; evals = EvalMap.empty
        }

    
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


    (****************************)
    (*  Subscribers management  *)
    (****************************)
    let subscribe engine clientid ?workspace selector (is_push: bool) (notifier:notify_subscriber) =
      (* TODO: access control *)
      let%lwt sel = to_absolute_selector engine clientid ?workspace selector in
      let feid = FeId.to_string clientid.feid in
      let sid = SessionId.to_string clientid.sid in
      let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng] %s/%s: subscribe %s" feid sid (Selector.to_string sel)) in
      MVar.read engine >>= fun self -> YAdminSpace.create_subscriber self.admin clientid sel is_push notifier

    let unsubscribe engine client sid =
      let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng]: unsubscribe %s" (SubscriberId.to_string sid)) in
      MVar.read engine >>= fun self -> YAdminSpace.remove_subscriber self.admin client sid


    (****************************)
    (*     Eval management      *)
    (****************************)
    let eval engine clientid ?workspace path get_on_eval =
      (* TODO: access control *)
      let%lwt pat = to_absolute_path engine clientid ?workspace path in
      let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng]: eval %s" (Path.to_string pat)) in
      (MVar.guarded engine 
      @@ fun self ->         
        let evals' = EvalMap.add pat get_on_eval self.evals in 
        MVar.return () {self with evals = evals'} )

    let remove_eval engine path =
      (* Note: this function is called via remove function which ensure the path is absolute *)
      (MVar.guarded engine 
      @@ fun self ->         
        let evals' = EvalMap.remove path self.evals in 
        MVar.return () {self with evals = evals'} )


    (*****************************)
    (*   Key/Value operations    *)
    (*****************************)
    let get_on_evals engine sel =
      let fallback path =
        remove_eval engine path >>= fun _ ->
        Lwt.return @@ Value.StringValue
          (Printf.sprintf "Error calling get(%s) on eval(%s): Access was removed"
          (Selector.to_string sel) (Path.to_string path))
      in
      let call_eval path (get_on_eval:get_on_eval) = get_on_eval path sel ~fallback
      in
      MVar.read engine >>= fun self ->
      let evals = EvalMap.filter (fun path _ -> Selector.is_matching_path path sel) self.evals in
      EvalMap.mapi call_eval evals |> EvalMap.bindings |> List.map (fun (p,v) -> v >|= (fun v -> (p,v))) |> LwtM.flatten

    let get engine client ?quorum ?workspace sel =
      (* TODO: access control *)
      (* TODO: transport with quorum *)
      let _ = ignore quorum in
      let%lwt sel = to_absolute_selector engine client ?workspace sel in
      MVar.read engine 
      >>= fun self ->
      match Selector.properties sel with
      | Some _ ->
        let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng]: get %s with properties => forward to evals" (Selector.to_string sel)) in
        get_on_evals engine sel
      | None ->
        let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng]: get %s => query storages" (Selector.to_string sel)) in
        YAdminSpace.get_storages_for_selector self.admin sel
        >>= fun storages -> List.map (fun store -> Storage.get store sel) storages
        |> Apero.LwtM.flatten
        >|= List.concat
      (* TODO? If in the future we accept Storages with conflicting paths,
        there might be duplicate keys from different Storages in this result.
        Shall we remove duplicates?? *)

    let put engine client ?quorum ?workspace path value =
      (* TODO: access control *)
      (* TODO: transport with quorum *)
      let _ = ignore quorum in
      let%lwt path = to_absolute_path engine client ?workspace path in
      let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng]: put %s" (Path.to_string path)) in
      MVar.read engine 
      >>= fun self ->      
      let _ = Lwt.return @@ YAdminSpace.notify_subscribers self.admin path value in
      YAdminSpace.get_storages_for_path self.admin path
      >>= Lwt_list.iter_p (fun store -> Storage.put store path value)

    let update engine client ?quorum ?workspace path value =
      (* TODO: access control *)
      (* TODO: transport with quorum *)
      let _ = ignore quorum in
      let%lwt path = to_absolute_path engine client ?workspace path in
      let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng]: update %s" (Path.to_string path)) in
      MVar.read engine 
      >>= fun self ->      
      let _ = Lwt.return @@ YAdminSpace.notify_subscribers self.admin path value in
      YAdminSpace.get_storages_for_path self.admin path
      >>= Lwt_list.iter_p (fun store -> Storage.update store path value)

    let remove engine client ?quorum ?workspace path =
      (* TODO: access control *)
      (* TODO: transport with quorum *)
      let _ = ignore quorum in
      let%lwt path = to_absolute_path engine client ?workspace path in
      MVar.read engine 
      >>= fun self ->      
      match EvalMap.find_opt path self.evals with
      | Some _ ->
        let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng]: remove %s (eval)" (Path.to_string path)) in
        (MVar.guarded engine 
        @@ fun self ->         
          let evals' = EvalMap.remove path self.evals in 
          MVar.return () {self with evals = evals'} )
      | None ->
        let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng]: remove %s" (Path.to_string path)) in
        YAdminSpace.get_storages_for_path self.admin path
        >>= Lwt_list.iter_p (fun store -> Storage.remove store path)



    let add_backend_TMP engine id be = 
      MVar.read engine >>= fun self -> YAdminSpace.add_backend_TMP self.admin id be

    let add_frontend_TMP engine id properties =
      MVar.read engine >>= fun self -> YAdminSpace.add_frontend_TMP self.admin id properties
 
  end

end
