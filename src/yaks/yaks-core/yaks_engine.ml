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

    val id : Yid.t

    val login : t -> ClientId.t -> properties -> unit Lwt.t
    val logout : t -> ClientId.t -> unit Lwt.t   

    val add_workspace : t -> ClientId.t -> Path.t -> WsId.t Lwt.t

    val get : t -> ClientId.t -> ?quorum:int -> ?workspace:WsId.t -> Selector.t -> (Path.t * Value.t) list  Lwt.t
    val put : t -> ClientId.t -> ?quorum:int -> ?workspace:WsId.t -> Path.t -> Value.t -> unit Lwt.t
    val update : t -> ClientId.t -> ?quorum:int -> ?workspace:WsId.t -> Path.t -> Value.t -> unit Lwt.t
    val remove : t -> ClientId.t -> ?quorum:int -> ?workspace:WsId.t -> Path.t -> unit Lwt.t

    val subscribe : t -> ClientId.t -> ?workspace:WsId.t -> Selector.t -> bool -> notify_subscriber -> SubscriberId.t Lwt.t  
    val unsubscribe : t -> ClientId.t -> SubscriberId.t -> unit Lwt.t  

    val eval : t -> ClientId.t -> ?workspace:WsId.t -> Path.t -> get_on_eval -> unit Lwt.t

    (* TODO: Temporary operations that should be replaced by put/get/remove usage *)
    val add_backend_TMP : t -> (module Backend) -> unit Lwt.t
    val add_frontend_TMP : t -> string -> properties -> unit Lwt.t
  end

  module Make (MVar: Apero.MVar) = struct

    module YAdminSpace = Yaks_admin_space.AdminSpace.Make(MVar)

    type state =
      { admin : YAdminSpace.t }

    type t = state MVar.t

    let id = Yid.make ()

    let make () =
      let _ = Logs_lwt.debug (fun m -> m "Creating Engine\n") in 
      MVar.create 
        { admin = YAdminSpace.make id }

    
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
    let eval engine clientid ?workspace path get_on_eval =
      (* TODO: access control *)
      let%lwt pat = to_absolute_path engine clientid ?workspace path in
      let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng]: eval %s" (Path.to_string pat)) in
      MVar.read engine >>= fun self -> YAdminSpace.create_eval self.admin clientid pat get_on_eval


    (*****************************)
    (*   Key/Value operations    *)
    (*****************************)

    let admin_prefix_id = "/_admin_/"^(Yid.to_string id)
    let admin_prefix_local = "/_admin_/local"

    let check_if_admin_selector sel =
      let pat = Selector.path sel in
      if Astring.is_prefix ~affix:admin_prefix_local pat then
        Astring.with_range ~first:(String.length admin_prefix_local) pat
        |> Astring.append admin_prefix_id
        |> Selector.of_string
        |> Option.return
      else if Astring.is_prefix ~affix:admin_prefix_id pat then
        Some sel
      else
        None

    let check_if_admin_path path =
      let pat = Path.to_string path in
      if Astring.is_prefix ~affix:admin_prefix_local pat then
        Astring.with_range ~first:(String.length admin_prefix_local) pat
        |> Astring.append admin_prefix_id
        |> Path.of_string
        |> Option.return
      else if Astring.is_prefix ~affix:admin_prefix_id pat then
        Some path
      else
        None

    let get engine client ?quorum ?workspace sel =
      (* TODO: access control *)
      (* TODO: transport with quorum *)
      let _ = ignore quorum in
      let%lwt sel = to_absolute_selector engine client ?workspace sel in
      match check_if_admin_selector sel with
      | Some sel -> MVar.read engine >>= fun self -> YAdminSpace.get self.admin client sel
      | None ->
        match Selector.properties sel with
        | Some _ ->
          let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng]: get %s with properties => forward to evals" (Selector.to_string sel)) in
          MVar.read engine >>= fun self ->
          YAdminSpace.get_on_evals self.admin client sel
        | None ->
          let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng]: get %s => query storages" (Selector.to_string sel)) in
          MVar.read engine >>= fun self ->
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
      match check_if_admin_path path with
      | Some path -> MVar.read engine >>= fun self -> YAdminSpace.put self.admin client path value
      | None ->
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
      match check_if_admin_path path with
      | Some path -> MVar.read engine >>= fun self -> YAdminSpace.remove self.admin client path
      | None ->
        (* NOTE: the engine doesnt know if the path to remove is an eval or belongs to some storages... *)
        (* Therefore, it tries to remove the path as an eval anyway, since it doesn't harm if it's not an eval *)
          let%lwt _ = Logs_lwt.debug (fun m -> m "[Yeng]: remove %s" (Path.to_string path)) in
          YAdminSpace.remove_eval self.admin client path >>= fun () ->
          YAdminSpace.get_storages_for_path self.admin path
          >>= Lwt_list.iter_p (fun store -> Storage.remove store path)


    let add_backend_TMP engine be = 
      MVar.read engine >>= fun self -> YAdminSpace.add_backend_TMP self.admin be

    let add_frontend_TMP engine id properties =
      MVar.read engine >>= fun self -> YAdminSpace.add_frontend_TMP self.admin id properties
 
  end

end
