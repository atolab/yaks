open Apero
open Yaks_core
open Yaks_access
open Cohttp
open LwtM.InfixM
open Yaks_user


module REST_Mirage (YEngine : Yaks_engine.SEngine.S) (CON:Conduit_mirage.S) = struct

  module Str = Re.Str

  module Server = Cohttp_mirage.Server(Conduit_mirage.Flow)
  (* module Channel = Mirage_channel_lwt.Make(Conduit_mirage.Flow) *)

  type config = { port : int }

  module TokenMap = Map.Make(String)

  type t = 
    { cfg: config
    ; engine: YEngine.t 
    ; stop: unit Lwt.t
    ; stopper: unit Lwt.u
    ; mutable request_counter: int64
    ; mutable tokens : User.Id.t TokenMap.t }

  (** TODO: This should also be a functor configured by the serialier  *)


  let yaks_control_keyword = "yaks"
  let yaks_control_uri_prefix = "/"^yaks_control_keyword

  let cookie_name_access_id = "is.yaks.access"
  let cookie_name_storage_id = "is.yaks.storage"
  let cookie_name_user_token = "is.yaks.user.token"
  let cookie_name_user_id = "is.yaks.user.id"
  let cookie_name_group_id = "is.yaks.group.id"
  let cookie_mame_group_rws = "is.yaks.group.rws"
  let cookie_mame_group_rs = "is.yaks.group.rs"
  let cookie_mame_group_ws = "is.yaks.group.ws"
  (**********************************)
  (*      helpers functions         *)
  (**********************************)
  let next_request_counter fe =
    fe.request_counter <- Int64.succ fe.request_counter;
    fe.request_counter

  let query_get_opt query name =
    let open Option.Infix in 
    List.find_opt (fun (n, _) -> n = name) query >>= fun (_, v) -> Some(v)

  let query_to_string query =
    List.map (fun (n,v) -> Printf.sprintf "%s=%s" n (String.concat "," v)) query
    |> String.concat "&"


  let properties_of_query  =  
    List.map (fun (k, ps) -> 
        let k, v = k, String.concat  ","  ps in     
        Property.make k v) 


  let set_cookie key value =
    let cookie = Cohttp.Cookie.Set_cookie_hdr.make (key, value) in
    Cohttp.Cookie.Set_cookie_hdr.serialize ~version:`HTTP_1_0 cookie

  let string_of_cookies header =
    Cookie.Cookie_hdr.extract header |>
    List.fold_left (fun acc (k, v) -> acc^" , "^k^"="^v) ""

  (**********************************)
  (*        Error replies           *)
  (**********************************)

  let not_implemented msg =
    Server.respond_string ~status:`Not_implemented ~body:msg ()

  let empty_path =
    Server.respond_string ~status:`Bad_request ~body:(
      "Welcome to YAKS REST API\n\n"^
      "Usage:\n\n"^
      "POST /yaks/access?path=[string]&cacheSize=[int]\n"^
      "   => creates an Access\n"
    )
      ()

  let forbidden s =
    Server.respond_error ~status:`Forbidden ~body:("Unauthorized access to  \""^s) ()

  let unsupported_uri path =
    Server.respond_error ~status:`Bad_request ~body:("No operation available on path: "^path) ()

  let unsupported_operation operation path =
    Server.respond_error ~status:`Bad_request ~body:("Operation "^(Code.string_of_method operation)^" not supported on path: "^path) ()

  let missing_query query_elts =
    Server.respond_error ~status:`Bad_request ~body:("Missing query elements (or wrong format): "^String.concat " , " query_elts) ()

  let insufficient_storage cache_size =
    Server.respond_error ~status:`Insufficient_storage ~body:("Insufficient storage for creation of an Access with cache size = "^(Int64.to_string cache_size)) ()

  let access_not_found id =
    Server.respond_error ~status:`Not_found ~body:("Access \""^id^"\" not found") ()

  let invalid_access id =
    Server.respond_error ~status:`Not_found ~body:("Invalid Access Id  \""^id) ()

  let invalid_access_id = invalid_access

  let invalid_storage_id id =
    Server.respond_error ~status:`Not_found ~body:("Invalid Storage Id  \""^id) ()

  let invalid_path p =
    Server.respond_error ~status:`Not_found ~body:("Invalid Storage Path  \""^p) ()

  let storage_not_found id =
    Server.respond_error ~status:`Not_found ~body:("Storage \""^id^"\" not found") ()

  let missing_cookie cookie_name =
    Server.respond_error ~status:`Precondition_failed ~body:("Missing cookie: "^cookie_name) ()

  let no_matching_key selector =
    Server.respond_error ~status:`Not_found ~body:("No key found matching selector: "^selector) ()

  let invalid_selector s =
    Server.respond_error ~status:`Not_found ~body:("Invalid Selector  \""^s) ()

  let bad_request s =
    Server.respond_error ~status:`Bad_request ~body:("Invalid Request  \""^s) ()

  let invalid_id id =
    Server.respond_error ~status:`Not_found ~body:("Invalid Id  \""^id) ()

  (**********************************)
  (*      Control operations        *)
  (**********************************)

  let create_access_with_id fe (path:Path.t) cache_size access_id user_id = 
    let aid = Access.Id.to_string access_id in 
    let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   create_access_with_id %s %s %Ld" aid (Path.to_string path) cache_size) in
    Lwt.try_bind 
      (fun () -> YEngine.create_access_with_id fe.engine path cache_size user_id access_id)
      (fun () ->      

         Logs_lwt.debug (fun m -> m "Created Access with id : %s responding client" aid) >>      
         let headers = Header.add_list (Header.init()) 
             [("Location", ".");
              (set_cookie cookie_name_access_id aid)] in
         Server.respond_string ~status:`Created ~headers ~body:"" ())
      (fun _ -> insufficient_storage cache_size)

  let create_access fe (path:Path.t) cache_size user_id = 
    let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   create_access %s %Ld"  (Path.to_string path) cache_size) in
    Lwt.try_bind 
      (fun () -> YEngine.create_access fe.engine path cache_size user_id )
      (fun access_id ->
         let aid = Access.Id.to_string access_id in  
         Logs_lwt.debug (fun m -> m "Created Access with id : %s responding client" aid) >>      
         let headers = Header.add_list (Header.init()) 
             [("Location", aid);
              (set_cookie cookie_name_access_id aid)] in
         Server.respond_string ~status:`Created ~headers ~body:"" ())
      (fun _ -> insufficient_storage cache_size)

  (* 
  Creating a user group 
  return the groupid as a cookie, this allow the creation of a user in the already created group
*)
  let create_group fe name rw_paths r_paths w_paths level = 
    let%lwt _ = Logs_lwt.debug (fun m -> m "[ENG]   create_group %s "  name) in
    let level = 
      match level with
      | 1L -> Group.Admins
      | _ -> Group.Users
    in
    Lwt.try_bind 
      (fun () -> YEngine.create_group fe.engine name rw_paths r_paths w_paths level )
      (fun group_id ->
         let aid = Group.Id.to_string group_id in  
         Logs_lwt.debug (fun m -> m "Created Group with id : %s responding client" aid) >>      
         let headers = Header.add_list (Header.init()) 
             [("Location", aid);
              (set_cookie cookie_name_group_id aid)] in
         Server.respond_string ~status:`Created ~headers ~body:"" ())
      (fun _ -> bad_request "Cannot create this group")

  (* 
  creating a new user 
  this return as a cookie the userid
  GB: can we authenticate users also using the tuple (userid, password)?
*)
  let create_user fe name password group = 
    let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   create_user %s "  name) in
    Lwt.try_bind 
      (fun () -> YEngine.create_user fe.engine name password group )
      (fun user_id ->
         let aid = User.Id.to_string user_id in  
         Logs_lwt.debug (fun m -> m "Created user with id : %s responding client" aid) >>      
         let headers = Header.add_list (Header.init()) 
             [("Location", aid);
              (set_cookie cookie_name_user_id aid)] in
         Server.respond_string ~status:`Created ~headers ~body:"" ())
      (fun _ -> bad_request "Cannot create this user")

  (* 
  authenticating the user using the tuple (username, password) and creating the session token
  token is used for authenticate the user in all the functions.
  The session token is managed by the frontend, the engine only tells if an user can or cannot authenticate
  This token will be destroyed when the user logout from YAKS
 *)
  let authenticate_user fe name password = 
    let%lwt _ = Logs_lwt.debug (fun m -> m "[FER] authenticate_user %s "  name) in
    Lwt.try_bind 
      (fun () -> YEngine.authenticate_user fe.engine name password )
      (fun user_id ->
         let token = Apero.Uuid.to_string @@ Apero.Uuid.next_id () in
         fe.tokens <- TokenMap.add token user_id fe.tokens;
         let headers = Header.add_list (Header.init()) 
             [("Location", token);
              (set_cookie cookie_name_user_token token)] in
         Server.respond_string ~status:`Created ~headers ~body:"" ())
      (fun _ -> bad_request "Cannot authenticate this user")

  (* logout the user 
     GB: this should call a function of the engine to also destroy all the access created by this user?
  *)
  let deauthenticate_user fe token = 
    let%lwt _ = Logs_lwt.debug (fun m -> m "[FER] deauthenticate_user %s "  token) in
    Lwt.try_bind 
      (fun () -> 
         match TokenMap.mem token fe.tokens with
         | true -> fe.tokens <- TokenMap.remove token fe.tokens; Lwt.return token (* Here should tell the engine to eventually destroy all access *)
         | false ->  Lwt.fail @@ YException (`InvalidParameters)
      )
      (fun token -> 
         let headers = Header.add_list (Header.init()) 
             [("Location", token);
              (set_cookie cookie_name_user_token token)] in
         Server.respond_string ~status:`Created ~headers ~body:"" ())
      (fun _ -> bad_request "Cannot logout this user")

  let get_access fe access_id =
    let aid = Access.Id.to_string access_id in 
    let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   get_access %s" aid) in

    match%lwt YEngine.get_access fe.engine access_id with 
    | Some _ -> 
      let headers = Header.add_list (Header.init()) 
          [("Location", aid);
           (set_cookie cookie_name_access_id aid)] in
      Server.respond_string ~status:`Created ~headers ~body:"" ()
    | None -> 
      access_not_found aid 

  let dispose_access fe access_id = 
    (* Intentionally do not give hint when an access_id does not exist.
       The operation always succeeds *)
    YEngine.dispose_access fe.engine access_id 
    >>= fun () -> Server.respond_string ~status:`No_content ~body:"" ()

  let create_storage_with_id fe path properties storage_id = 
    YEngine.create_storage_with_id fe.engine path properties storage_id
    >>= fun () -> 
    let sid = StorageId.to_string storage_id in
    let headers = Header.add_list (Header.init())
        [("Location",  ".");
         (set_cookie cookie_name_storage_id sid)] in
    Server.respond_string ~status:`Created ~headers ~body:"" ()

  let create_storage fe path properties = 
    YEngine.create_storage fe.engine path properties 
    >>= fun id -> 
    let sid = StorageId.to_string id in
    let headers = Header.add_list (Header.init())
        [("Location",  sid);
         (set_cookie cookie_name_storage_id sid)] in
    Server.respond_string ~status:`Created ~headers ~body:"" ()

  let dispose_storage fe storage_id = 
    (* Intentionally do not give hint when an access_id does not exist.
       The operation always succeeds *)
    YEngine.dispose_storage fe.engine storage_id 
    >>= fun () -> Server.respond_string ~status:`No_content ~body:"" ()

  let dispose_group fe group_id = 
    YEngine.dispose_group fe.engine group_id >>= 
    fun () -> Server.respond_string ~status:`No_content ~body:"" ()

  let dispose_user fe user_id = 
    YEngine.dispose_user fe.engine user_id >>= 
    fun () -> Server.respond_string ~status:`No_content ~body:"" ()

  (* @AC: We should add subscriptions only once we'll have a front-end that can 
          deal with them. Notice that subscriptions introduce potentially concurrent
          notification from the Engine to the front-end. 

          Perhaps the right way to do this is to have the front-end 
          provide a "push" function to the engine and then deal with it.

  *)

(*
let subscribe fe access_id selector = 
  let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   subscribe %s %s" access_id selector) in
  let msg = Create { 
      cid = next_request_counter fe;
      entity = Subscriber {access_id; selector; push=false};
      entity_id = Auto
    }
  in
  push_to_engine fe msg
  >>= function
  | Ok {cid=_; entity_id=SubscriberId(sid)} ->
    let headers = Header.add_list (Header.init()) [("Location",Int64.to_string sid);] in
    Server.respond_string ~status:`Created ~headers ~body:"" ()
  | Error {cid=_; reason=412} ->
    access_not_found access_id
  | x ->
    unexpected_reply x

let get_subscriptions fe access_id =
  let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   get_subscriptions %s" access_id) in
  let msg = Get { 
      cid = next_request_counter fe;
      entity_id = Yaks;
      key = "access/"^access_id^"/sub";
      encoding = Some(`Json)
    }
  in
  push_to_engine fe msg
  >>= function
  | Values {cid=_; encoding=`Json; values} ->
    Server.respond_string ~status:`OK ~body:(json_string_of_values values) ()
  | Error {cid=_; reason=404} ->
    access_not_found access_id
  | x ->
    unexpected_reply x


let unsubscribe fe access_id sub_id =
  let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   unsubscribe %s %Ld" access_id sub_id) in
  let msg = Dispose { 
      cid = next_request_counter fe;
      entity_id = SubscriberId(sub_id)
    }
  in
  push_to_engine fe msg
  >>= function
  | Ok {cid=_; entity_id=_} ->
    Server.respond_string ~status:`No_content ~body:"" ()
  | Error {cid=_; reason=404} ->
    access_not_found access_id
  | x ->
    unexpected_reply x *)

  (**********************************)
  (*      Key/Value operations      *)
  (**********************************)

  let status_ok = `Ok

  let json_string_of_key_values (kvs : (string * Value.t) list) =
    kvs
    |> List.map (fun (key, value) -> Printf.sprintf "\"%s\":%s" key  (Value.to_string value))
    |> String.concat ","
    |> Printf.sprintf "{%s}"

  let get_key_value fe (access_id:Access.Id.t) (selector: Selector.t) =
    let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   get_key_value %s %s" (Access.Id.to_string access_id) (Selector.to_string selector)) in
    YEngine.get fe.engine access_id selector
    >>= fun (kvs) -> 
    Server.respond_string ~status:`OK ~body:(json_string_of_key_values kvs) ()    


  let put fe (access_id:Access.Id.t) (selector: Selector.t) (value: Value.t) =
    let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   put %s %s\n%s" (Access.Id.to_string access_id) (Selector.to_string selector) (Value.to_string value)) in
    Lwt.try_bind 
      (fun () -> 
         let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   put calling YEngine.put") in
         YEngine.put fe.engine access_id selector value) 
      (fun () -> Server.respond_string ~status:`No_content ~body:"" ())
      (fun _ -> no_matching_key (Selector.to_string selector))


  let put_delta  fe (access_id:Access.Id.t) selector delta =  
    let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   put_delta %s %s\n%s" (Access.Id.to_string access_id) (Selector.to_string selector) (Value.to_string delta)) in  
    Lwt.try_bind 
      (fun () -> 
         let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   put calling YEngine.put_delta") in
         YEngine.put_delta fe.engine access_id selector delta) 
      (fun () -> Server.respond_string ~status:`No_content ~body:"" ())
      (fun _ -> no_matching_key (Selector.to_string selector))

(*
let remove_key_value fe access_id selector =
  let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   put_delta_key_value %s %s" access_id selector) in
  let msg = Remove { 
      cid = next_request_counter fe;
      access_id = Access.Id(access_id);
      key = selector;
    }
  in
  push_to_engine fe msg
  >>= function
  | Ok {cid=_; _} ->
    Server.respond_string ~status:`No_content ~body:"" ()
  | Error {cid=_; reason=404} ->
    no_matching_key selector
  | x ->
    unexpected_reply x *)





  (**********************************)
  (*   HTTP requests dispatching    *)
  (**********************************)
  (*let execute_control_operation fe meth path query headers body =  *)
  let execute_control_operation fe meth path query headers _ =
    let%lwt _ = Logs_lwt.debug (fun m -> m "-- execute_control_operation --" ) in  
    match (meth, path) with
    (* PUT /yaks/group/ *)
    | (`PUT, ["yaks"; "group"]) -> (
        let open Option.Infix in
        (* User should be authenticated before creating a group
           only admin users can create a group *)
        (* retrieve the user id from the access token *)
        let user_id : User.Id.t option = 
          (Cookie.Cookie_hdr.extract headers
           |> List.find_opt (fun (key, _) -> key = cookie_name_user_token) 
              >== fun (_, value) -> value) >>= (fun aid -> TokenMap.find_opt aid fe.tokens) 
        in
        let rws : string option = 
          (Cookie.Cookie_hdr.extract headers
           |> List.find_opt (fun (key, _) -> key = cookie_mame_group_rws) 
              >== fun (_, value) -> value) >>= (fun aid -> Some aid) 
        in  
        let rs : string option = 
          (Cookie.Cookie_hdr.extract headers
           |> List.find_opt (fun (key, _) -> key = cookie_mame_group_rs) 
              >== fun (_, value) -> value) >>= (fun aid -> Some aid) 
        in  
        let ws : string option = 
          (Cookie.Cookie_hdr.extract headers
           |> List.find_opt (fun (key, _) -> key = cookie_mame_group_ws) 
              >== fun (_, value) -> value) >>= (fun aid -> Some aid) 
        in   
        let name =  query_get_opt query "name" >== List.hd in
        let isadmin = 
          match query_get_opt query "is_admin" >== List.hd >>= Int64.of_string_opt with
          | Some i -> i
          | None -> 0L
        in
        match user_id with
        (* | None -> forbidden @@ "yaks/group/"
           | Some _ ->  *)
        | _ -> 
          match (name,rws,rs,ws) with
          | (None,None,None,None) -> missing_query (["name:string"])
          | (None, _,_,_)    -> missing_query (["name:string"])
          | (_,None,_, _)    -> missing_cookie "yaks.rws"
          | (_,_,None, _)    -> missing_cookie "yaks.rs"
          | (_,_,_, None)    -> missing_cookie "yaks.ws"
          | (Some(name), Some(rw_paths), Some(r_paths),Some(w_paths)) ->
            let to_list var = 
              let s = String.sub var 1 ((String.length var)-2) in
              let l_s = Str.split (Str.regexp ",") s in
              List.map (fun e -> Str.global_replace (Str.regexp "\"") "" e) l_s
            in
            create_group fe name (Selector.of_string_list @@ to_list rw_paths) (Selector.of_string_list @@ to_list r_paths) (Selector.of_string_list @@ to_list w_paths) isadmin
      )
    (* DELETE /yaks/group/id *)
    | (`DELETE, ["yaks"; "group"; id]) -> (
        match (Group.Id.of_string id) with
        | Some id -> dispose_group fe id
        | None -> invalid_id id
      )
    (* PUT /yaks/user/?name&group*)
    | (`PUT, ["yaks"; "user"]) -> (
        let open Option.Infix in
        (* Very dummy authentication method *)
        (* User should be authenticated before creating a group
           only admin users can create other users *)
        (* retrieve the user id from the access token *)
        let user_id : User.Id.t option = 
          (Cookie.Cookie_hdr.extract headers
           |> List.find_opt (fun (key, _) -> key = cookie_name_user_token) 
              >== fun (_, value) -> value) >>= (fun aid -> TokenMap.find_opt aid fe.tokens) 
        in
        let pwd : string option = 
          (Cookie.Cookie_hdr.extract headers
           |> List.find_opt (fun (key, _) -> key = "yaks.user.pwd") 
              >== fun (_, value) -> value) >>= (fun aid -> Some aid) 
        in  
        let name =  query_get_opt query "name" >== List.hd in
        let group =  query_get_opt query "group" >== List.hd in
        match user_id with
        (* | None -> forbidden @@ "yaks/user/"
           | Some _ ->  *)
        | _ -> 
          match (name,group,pwd) with
          | (None,None,None) -> missing_query (["name:string"; "group:string"])
          | (None, _,_)    -> missing_query (["name:string"])
          | (_,None,_)    -> missing_query (["name:string"])
          | (_,_,None)    -> missing_cookie "yaks.user.pwd"
          | (Some(name), Some(group), Some(password)) ->
            match Group.Id.of_string group with
            | Some g -> create_user fe name password g
            | None -> bad_request "Malformed Group id"
      )
    (* POST /yaks/authenticate ? name *)
    | (`POST, ["yaks"; "authenticate";]) -> (
        let open Option.Infix in
        let pwd : string option = 
          (Cookie.Cookie_hdr.extract headers
           |> List.find_opt (fun (key, _) -> key = "yaks.user.pwd") 
              >== fun (_, value) -> value) >>= (fun aid -> Some aid) 
        in  
        let name =  query_get_opt query "name" >== List.hd in
        match (name,pwd) with
        | (None, None)    -> missing_query (["name:string"])
        | (None, _)    -> missing_query (["name:string"])
        | (_,None)    -> missing_cookie "yaks.user.pwd"
        | (Some(name), Some(password)) ->
          authenticate_user fe name password
      )
    (* POST /yaks/deauthenticate *)
    | (`POST, ["yaks"; "deauthenticate";]) -> (
        let open Option.Infix in
        let token : string option = 
          (Cookie.Cookie_hdr.extract headers
           |> List.find_opt (fun (key, _) -> key = cookie_name_user_token) 
              >== fun (_, value) -> value) >>= (fun aid -> Some aid) 
        in  
        match token with
        | None -> missing_cookie cookie_name_user_token
        | Some tok ->
          deauthenticate_user fe tok 
      )
    (* DELETE /yaks/user/id *)
    | (`DELETE, ["yaks"; "user"; id]) -> (
        match (User.Id.of_string id) with
        | Some id -> dispose_user fe id
        | None -> invalid_id id
      )
    (* POST /yaks/access ? path & cacheSize *)
    | (`POST, ["yaks"; "access"]) -> (
        let open Option.Infix in
        let user_id : User.Id.t option = 
          (Cookie.Cookie_hdr.extract headers
           |> List.find_opt (fun (key, _) -> key = cookie_name_user_token) 
              >== fun (_, value) -> value) >>= (fun aid -> TokenMap.find_opt aid fe.tokens)
        in  
        let access_path =  query_get_opt query "path" >== List.hd in
        let cache_size = query_get_opt query "cacheSize" >== List.hd >>= Int64.of_string_opt in
        match user_id with
        | None -> forbidden @@ "yaks/access/"
        | Some uid -> 
          match (access_path, cache_size) with
          | (None, None) -> missing_query (["path:string"; "cacheSize:int"])
          | (None, _)    -> missing_query (["path:string"])
          | (_, None)    -> missing_query (["cacheSize:int"])
          | (Some(path), Some(cache_size)) -> (
              match Path.of_string path with 
              | Some p -> create_access fe p cache_size uid
              | None -> invalid_path path)
      )
    (* PUT /yaks/access/id ? path & cacheSize *)
    | (`PUT, ["yaks"; "access"; id]) -> (
        let open Option.Infix in
        (* Very dummy authentication method *)
        let user_id : User.Id.t option = 
          (Cookie.Cookie_hdr.extract headers
           |> List.find_opt (fun (key, _) -> key = cookie_name_user_token) 
              >== fun (_, value) -> value) >>= (fun aid -> TokenMap.find_opt aid fe.tokens) 
        in  
        let access_path =  query_get_opt query "path" >== List.hd in
        let cache_size = query_get_opt query "cacheSize" >== List.hd >>= Int64.of_string_opt in
        match user_id with
        | None -> forbidden @@ "yaks/access/"^id
        | Some uid -> 
          match (access_path, cache_size) with
          | (None, None) -> missing_query (["path:string"; "cacheSize:int"])
          | (None, _)    -> missing_query (["path:string"])
          | (_, None)    -> missing_query (["cacheSize:int"])
          | (Some(path), Some(cache_size)) -> 
            match Access.Id.of_string id with 
            | Some access_id -> (
                match Path.of_string path with 
                |Some p -> create_access_with_id fe p cache_size access_id uid
                | None -> invalid_path path)
            | None -> invalid_access id
      )
    (* GET /yaks/access *)
    | (`GET, ["yaks"; "access"]) -> (      
        (* get_access fe  *)

        unsupported_uri "/yaks/access"
      )
    (* GET /yaks/access/id *)
    | (`GET, ["yaks"; "access"; id]) -> (
        match Access.Id.of_string id with 
        | Some access_id -> get_access fe access_id
        | None -> invalid_access id

      )
    (* Dispose /yaks/access/id *)
    | (`DELETE, ["yaks"; "access"; id]) -> (
        unsupported_uri id
        (* dispose_access fe id *)
      )
    (* POST /yaks/storages ? path & options... *)
    | (`POST, ["yaks"; "storages"]) -> (
        let open Option.Infix in
        let _ : User.Id.t option = (* user_id *)
          (Cookie.Cookie_hdr.extract headers
           |> List.find_opt (fun (key, _) -> key = cookie_name_user_token) 
              >== fun (_, value) -> value) >>= (fun aid -> TokenMap.find_opt aid fe.tokens) 
        in 
        let storage_path =  query_get_opt query "path" >== List.hd in
        match storage_path with
        | None -> missing_query (["path:string"])
        | Some(path) -> 
          (match Path.of_string path with 
           | Some p -> 
             let properties = query |> List.filter (fun (n, _) -> n != "path") |> properties_of_query in        
             create_storage fe p properties
           | None -> invalid_path path)
      )
    (* PUT /yaks/storages/id ? path & options... *)
    | (`PUT, ["yaks"; "storages"; id ]) -> (
        let open Option.Infix in
        let _ : User.Id.t option = (* user_id *)
          (Cookie.Cookie_hdr.extract headers
           |> List.find_opt (fun (key, _) -> key = cookie_name_user_token) 
              >== fun (_, value) -> value) >>= (fun aid -> TokenMap.find_opt aid fe.tokens) 
        in 
        let storage_path =  query_get_opt query "path" >== List.hd in
        match storage_path with
        | None -> missing_query (["path:string"])
        | Some(path) -> 
          let properties = query |> List.filter (fun (n, _) -> n != "path") |> properties_of_query in
          (match StorageId.of_string id with 
           | Some storage_id -> 
             (match Path.of_string path with 
              | Some p -> create_storage_with_id fe p properties storage_id 
              | None -> invalid_path path)

           | None -> invalid_storage_id id)
          (* unsupported_uri "ZZZ" *)
      )
    (* GET /yaks/storages *)
    | (`GET, ["yaks"; "storages"]) -> (
        unsupported_uri "ZZZ"
        (* get_storage fe *)
      )
    (* GET /yaks/storages/id *)
    | (`GET, ["yaks"; "storages"; id]) -> (
        unsupported_uri id
        (* get_storage fe ~id *)
      )
    (* Dispose /yaks/storages/id *)
    | (`DELETE, ["yaks"; "storages"; id]) -> (
        unsupported_uri id
        (* dispose_storage fe id *)
      )
    (* POST /yaks/access/id/subs *)
    | (`POST, ["yaks"; "access"; aid; "subs"]) -> (
        let open Option.Infix in
        let _ : User.Id.t option = (* user_id *)
          (Cookie.Cookie_hdr.extract headers
           |> List.find_opt (fun (key, _) -> key = cookie_name_user_token) 
              >== fun (_, value) -> value) >>= (fun aid -> TokenMap.find_opt aid fe.tokens) 
        in 
        let selector =  query_get_opt query "selector" >== List.hd in
        match selector with
        | None -> missing_query (["selector:string"])
        | Some((*selector*) _) -> 
          unsupported_uri aid
          (* subscribe fe aid selector *)
      )
    (* Otherwise... *)
    | (_, _) -> String.concat "/" path |> unsupported_uri

  (* 
  @AC: Julien, is this a selector or a path? The naming is inconsistent and 
  conceptually it should be a selector. It would be good to know if that is the 
  case and make changes accordingly
 *)
  let execute_data_operation fe meth (selector: Selector.t) headers body =
    let%lwt _ = Logs_lwt.debug (fun m -> m "(-- execute_data_operation --" ) in 
    let open Apero.Option.Infix in

    let user_id : User.Id.t option = 
      (Cookie.Cookie_hdr.extract headers
       |> List.find_opt (fun (key, _) -> key = cookie_name_user_token) 
          >== fun (_, value) -> value) >>= (fun aid -> TokenMap.find_opt aid fe.tokens) 
    in
    let access_id : Access.Id.t option = 
      (Cookie.Cookie_hdr.extract headers
       |> List.find_opt (fun (key, _) -> key = cookie_name_access_id)
          >== fun (_, value) -> value) >>= (fun aid -> Access.Id.of_string aid) 
    in  

    match user_id with
    | None -> forbidden @@ Selector.to_string selector
    | Some _ -> (* Should be passed to all functions? *)
      match (meth, access_id) with
      | (_, None) ->
        missing_cookie cookie_name_access_id
      | (`GET, Some(aid)) ->
        let%lwt _ = Logs_lwt.debug (fun m -> m "(-- get_key_value --" ) in 
        get_key_value fe aid selector
      | (`PUT, Some(aid)) ->
        let%lwt value = Cohttp_lwt.Body.to_string body in
        put fe aid selector (Value.JSonValue value)     

      | (`PATCH, Some(aid)) ->
        let%lwt value = Cohttp_lwt.Body.to_string body in
        put fe aid selector (Value.JSonValue value)
      (* put_delta_key_value fe aid selector value *)
      | (`DELETE, Some(_)) -> unsupported_operation meth (Selector.to_string selector)
      (* remove_key_value fe aid selector *)
      | _ -> unsupported_operation meth (Selector.to_string selector)


  let execute_http_request fe req body =
    (* let open Lwt in *)
    let meth = req |> Request.meth in
    let uri = req |> Request.uri in
    let path = uri |> Uri.path  in
    let query = uri |> Uri.query in
    let headers = req |> Request.headers in
    let%lwt _ = Logs_lwt.debug (fun m -> m "[FER] HTTP req: %s %s?%s with cookie: %s" 
                                   (Code.string_of_method meth) path (query_to_string query)
                                   (Cookie.Cookie_hdr.extract headers
                                    |> List.find_opt (fun (key, _) -> String.starts_with "is.yaks" key)
                                    |> function | Some(k,v) -> k^"="^v | _ -> ""))
    in
    if path = "/" then
      empty_path
    else if String.length path >= 5 && String.sub path 0 5 = yaks_control_uri_prefix then
      let normalized_path = path |> String.split_on_char '/' |> List.filter (fun s -> String.length s > 0) in
      execute_control_operation fe meth normalized_path query headers body
    else
      (match Selector.of_string path with 
       | Some selector -> execute_data_operation fe meth selector headers body
       | None -> invalid_path path)


  let create cfg engine = 
    let _ = Logs_lwt.debug (fun m -> m "[FER] REST-FE preparing HTTP server") in
    let stop, stopper = Lwt.wait () in
    { cfg; engine; stop; stopper; request_counter=0L; tokens= TokenMap.empty }

  let start fe conduit =
    let _ = Logs_lwt.debug (fun m -> m "[FER] REST-FE starting HTTP server on port %d" fe.cfg.port) in
    let callback _conn req body = execute_http_request fe req body in
    (* let u = Cohttp_lwt_unix.Server.create ~stop:fe.stop  ~mode:(`TCP (`Port fe.cfg.port)) (Cohttp_lwt_unix.Server.make ~callback ()) in u *)
    let spec = Server.make ~callback () in 
    let tcp_server = `TCP fe.cfg.port in
    CON.listen conduit tcp_server (Server.listen spec)


  let stop fe =
    let _ = Logs_lwt.debug (fun m -> m "[FER] REST-FE stopping HTTP server") in
    Lwt.wakeup_later fe.stopper ()

end