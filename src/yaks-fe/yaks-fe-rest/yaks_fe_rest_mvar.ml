open Apero
open Yaks_core

open Cohttp
open Cohttp_lwt_unix
(* open Lwt *)
open LwtM.InfixM
type config = { port : int }

type t = 
  { cfg: config
  ; engine: SEngine.t Lwt_mvar.t 
  ; stop: unit Lwt.t
  ; stopper: unit Lwt.u
  ; mutable request_counter: int64 }

(** TODO: This should also be a functor configured by the serialier  *)


let yaks_control_keyword = "yaks"
let yaks_control_uri_prefix = "/"^yaks_control_keyword

let cookie_name_access_id = "is.yaks.access"
let cookie_name_storage_id = "is.yaks.storage"

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


(* let push_to_engine fe msg =
  let%lwt _ = Logs_lwt.debug (fun m -> m "[FER] send to engine %s" (string_of_message msg)) in
  let (promise, resolver) = Lwt.task () in
  let on_reply = fun reply ->
    let%lwt _ = Logs_lwt.debug (fun m -> m "[FER] recv from engine %s" (string_of_message reply)) in
    Lwt.return (Lwt.wakeup_later resolver reply)
  in
  let%lwt _ = fe.engine_mailbox <!> (None, (EventWithHandler (msg, on_reply))) in
  promise *)



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

let unsupported_uri path =
  Server.respond_error ~status:`Bad_request ~body:("No operation available on path: "^path) ()

let unsupported_operation operation path =
  Server.respond_error ~status:`Bad_request ~body:("Operation "^(Code.string_of_method operation)^" not supported on path: "^path) ()

let missing_query query_elts =
  Server.respond_error ~status:`Bad_request ~body:("Missing query elements (or wrong format): "^String.concat " , " query_elts) ()

let unexpected_reply reply =
  Server.respond_error ~status:`Internal_server_error ~body:("Unexpected reply from engine:"^(string_of_message reply)) ()

let insufficient_storage cache_size =
  Server.respond_error ~status:`Insufficient_storage ~body:("Insufficient storage for creation of an Access with cache size = "^(Int64.to_string cache_size)) ()

let access_not_found id =
  Server.respond_error ~status:`Not_found ~body:("Access \""^id^"\" not found") ()

let invalid_access id =
  Server.respond_error ~status:`Not_found ~body:("Invalid Access Id  \""^id) ()

let invalid_access_id = invalid_access

let invalid_storage_id id =
  Server.respond_error ~status:`Not_found ~body:("Invalid Storage Id  \""^id) ()

let storage_not_found id =
  Server.respond_error ~status:`Not_found ~body:("Storage \""^id^"\" not found") ()

let unkown_storage_type properties =
  Server.respond_error ~status:`Not_implemented ~body:("No implementation of Storage for properties: "^(string_of_properties properties)) ()

let missing_cookie cookie_name =
  Server.respond_error ~status:`Precondition_failed ~body:("Missing cookie: "^cookie_name) ()

let no_matching_key selector =
  Server.respond_error ~status:`Not_found ~body:("No key found matching selector: "^selector) ()



(**********************************)
(*      Control operations        *)
(**********************************)

let create_access_with_id fe (path:Path.t) cache_size access_id= 
  let aid = AccessId.to_string access_id in 
  let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   create_access_with_id %s %s %Ld" aid (Path.to_string path) cache_size) in
  Lwt.try_bind 
    (fun () -> SEngine.create_access_with_id fe.engine path cache_size access_id)
    (fun () ->      
      
      Logs_lwt.debug (fun m -> m "Created Access with id : %s responding client" aid) >>      
      let headers = Header.add_list (Header.init()) 
        [("Location", ".");
          (set_cookie cookie_name_access_id aid)] in
        Server.respond_string ~status:`Created ~headers ~body:"" ())
    (fun _ -> insufficient_storage cache_size)

let create_access fe (path:Path.t) cache_size =  
  let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   create_access %s %Ld"  (Path.to_string path) cache_size) in
  Lwt.try_bind 
    (fun () -> SEngine.create_access fe.engine path cache_size)
    (fun access_id ->
      let aid = AccessId.to_string access_id in  
      Logs_lwt.debug (fun m -> m "Created Access with id : %s responding client" aid) >>      
      let headers = Header.add_list (Header.init()) 
        [("Location", aid);
          (set_cookie cookie_name_access_id aid)] in
        Server.respond_string ~status:`Created ~headers ~body:"" ())
    (fun _ -> insufficient_storage cache_size)


let get_access fe access_id =
  let aid = AccessId.to_string access_id in 
  let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   get_access %s" aid) in
  
  match%lwt SEngine.get_access fe.engine access_id with 
  | Some _ -> 
    let headers = Header.add_list (Header.init()) 
        [("Location", aid);
          (set_cookie cookie_name_access_id aid)] in
        Server.respond_string ~status:`Created ~headers ~body:"" ()
  | None -> 
    access_not_found aid 

(*
let dispose_access fe id =
  let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   dispose_access %s" id) in
  let msg = Dispose { 
      cid = next_request_counter fe;
      entity_id = AccessId(id)
    }
  in
  push_to_engine fe msg
  >>= function
  | Ok{cid=_; entity_id=_} ->
    Server.respond_string ~status:`No_content ~body:"" ()
  | Error{cid=_; reason=404} ->
    access_not_found id
  | x ->
    unexpected_reply x *)

let create_storage_with_id fe path properties storage_id = 
    SEngine.create_storage_with_id fe.engine (Path.of_string path) properties storage_id
    >>= fun () -> 
    let sid = StorageId.to_string storage_id in
    let headers = Header.add_list (Header.init())
        [("Location",  ".");
         (set_cookie cookie_name_storage_id sid)] in
    Server.respond_string ~status:`Created ~headers ~body:"" ()

let create_storage fe path properties = 
  SEngine.create_storage fe.engine (Path.of_string path) properties 
  >>= fun id -> 
    let sid = StorageId.to_string id in
    let headers = Header.add_list (Header.init())
        [("Location",  sid);
         (set_cookie cookie_name_storage_id sid)] in
    Server.respond_string ~status:`Created ~headers ~body:"" ()

(* let create_storage fe ?id path properties =
  let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   create_storage %s %s" (Option.get_or_default id "?") path) in
  let msg = Create { 
      cid = next_request_counter fe;
      entity = Storage {path; properties};
      entity_id = match id with None -> Auto | Some(i) -> StorageId(i)
    }
  in
  push_to_engine fe msg
  >>= function
  | Ok {cid=_; entity_id=StorageId(sid)} ->
    let headers = Header.add_list (Header.init())
        [("Location", match id with None -> sid | Some(_) -> ".");
         (set_cookie cookie_name_storage_id sid)] in
    Server.respond_string ~status:`Created ~headers ~body:"" ()
  | Error {cid=_; reason=501} ->
    unkown_storage_type properties
  | x ->
    unexpected_reply x

let get_storage ?id fe =
  let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   get_storage %s" (Option.get_or_default id "?")) in
  let msg = Get { 
      cid = next_request_counter fe;
      entity_id = Yaks;
      key = "storages"^(Option.get_or_default id "");
      encoding = Some(`Json)
    }
  in
  push_to_engine fe msg
  >>= function
  | Values {cid=_; encoding=`Json; values} ->
    Server.respond_string ~status:`OK ~body:(json_string_of_values values) ()
  | Error {cid=_; reason=404} ->
    storage_not_found (Option.get_or_default id "NO_ID!!!")
  | x ->
    unexpected_reply x

let dispose_storage fe id =
  let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   dispose_storage %s" id) in
  let msg = Dispose { 
      cid = next_request_counter fe;
      entity_id = StorageId(id)
    }
  in
  push_to_engine fe msg
  >>= function
  | Ok {cid=_; entity_id=_} ->
    Server.respond_string ~status:`No_content ~body:"" ()
  | Error {cid=_; reason=404} ->
    storage_not_found id
  | x ->
    unexpected_reply x

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

let get_key_value fe access_id selector =
  Logs_lwt.debug (fun m -> m "[FER]   get_key_value %s %s" access_id selector) >>
  match (AccessId.of_string access_id) with 
  | Some aid ->  
    SEngine.get fe.engine aid (Selector.of_string selector ) 
    >>= fun (_, values) -> 
      Server.respond_string ~status:`OK ~body:(json_string_of_values values) ()
  | None -> invalid_access access_id
  
  

let put_key_value fe access_id selector value =
  Logs_lwt.debug (fun m -> m "[FER]   put_key_value %s %s\n%s" access_id selector value) >> 
  match AccessId.of_string access_id with 
  | Some aid -> 
    Lwt.try_bind 
    (fun () -> SEngine.put fe.engine aid (KeyValue.make selector value)) 
    (fun () -> Server.respond_string ~status:`No_content ~body:"" ())
    (fun _ -> no_matching_key selector)
  | None -> invalid_access access_id
  
(* let put_delta_key_value fe access_id selector delta_value =
  let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   put_delta_key_value %s %s" access_id selector) in
  let msg = Patch { 
      cid = next_request_counter fe;
      access_id = AccessId(access_id);
      key = selector;
      value=delta_value
    }
  in
  push_to_engine fe msg
  >>= function
  | Ok {cid=_; _} ->
    Server.respond_string ~status:`No_content ~body:"" ()
  | Error {cid=_; reason=404} ->
    no_matching_key selector
  | x ->
    unexpected_reply x

let remove_key_value fe access_id selector =
  let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   put_delta_key_value %s %s" access_id selector) in
  let msg = Remove { 
      cid = next_request_counter fe;
      access_id = AccessId(access_id);
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
let execute_control_operation fe meth path query _ _ =
  let%lwt _ = Logs_lwt.debug (fun m -> m "-- execute_control_operation --" ) in  
  match (meth, path) with
  (* POST /yaks/access ? path & cacheSize *)
  | (`POST, ["yaks"; "access"]) -> (
      let open Option.Infix in
      let access_path =  query_get_opt query "path" >== List.hd in
      let cache_size = query_get_opt query "cacheSize" >== List.hd >>= Int64.of_string_opt in
      match (access_path, cache_size) with
      | (None, None) -> missing_query (["path:string"; "cacheSize:int"])
      | (None, _)    -> missing_query (["path:string"])
      | (_, None)    -> missing_query (["cacheSize:int"])
      | (Some(path), Some(cache_size)) -> create_access fe (Path.of_string path) cache_size
    )
  (* PUT /yaks/access/id ? path & cacheSize *)
  | (`PUT, ["yaks"; "access"; id]) -> (
      let open Option.Infix in
      let access_path =  query_get_opt query "path" >== List.hd in
      let cache_size = query_get_opt query "cacheSize" >== List.hd >>= Int64.of_string_opt in
      match (access_path, cache_size) with
      | (None, None) -> missing_query (["path:string"; "cacheSize:int"])
      | (None, _)    -> missing_query (["path:string"])
      | (_, None)    -> missing_query (["cacheSize:int"])
      | (Some(path), Some(cache_size)) -> 
        match AccessId.of_string id with 
        | Some access_id -> create_access_with_id fe (Path.of_string path) cache_size access_id
        | None -> invalid_access id
    )
  (* GET /yaks/access *)
  | (`GET, ["yaks"; "access"]) -> (      
      (* get_access fe  *)
      unsupported_uri "/yaks/access"
    )
  (* GET /yaks/access/id *)
  | (`GET, ["yaks"; "access"; id]) -> (
      match AccessId.of_string id with 
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
      let storage_path =  query_get_opt query "path" >== List.hd in
      match storage_path with
      | None -> missing_query (["path:string"])
      | Some(path) -> 
        let properties = query |> List.filter (fun (n, _) -> n != "path") |> properties_of_query in        
        create_storage fe path properties
    )
  (* PUT /yaks/storages/id ? path & options... *)
  | (`PUT, ["yaks"; "storages"; id ]) -> (
      let open Option.Infix in
      let storage_path =  query_get_opt query "path" >== List.hd in
      match storage_path with
      | None -> missing_query (["path:string"])
      | Some(path) -> 
        let properties = query |> List.filter (fun (n, _) -> n != "path") |> properties_of_query in
        match StorageId.of_string id with 
        | Some storage_id -> create_storage_with_id fe path properties storage_id 
        | None -> invalid_storage_id id
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
      let selector =  query_get_opt query "selector" >== List.hd in
      match selector with
      | None -> missing_query (["selector:string"])
      | Some((*selector*) _) -> 
        unsupported_uri aid
        (* subscribe fe aid selector *)
    )
  (* Otherwise... *)
  | (_, _) -> String.concat "/" path |> unsupported_uri


let execute_data_operation fe meth selector headers body =
  Logs_lwt.debug (fun m -> m "(-- execute_data_operation --" ) >> 
  let open Apero.Option.Infix in
  let access_id = 
    Cookie.Cookie_hdr.extract headers
    |> List.find_opt (fun (key, _) -> key = cookie_name_access_id)
       >== fun (_, value) -> value
  in
  let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]  request on access %s" (Option.get_or_default access_id "!!NO_ID!!")) in
  match (meth, access_id) with
  | (_, None) ->
    missing_cookie cookie_name_access_id
  | (`GET, Some(aid)) ->
    get_key_value fe aid selector
  | (`PUT, Some(aid)) ->
    let%lwt value = Cohttp_lwt.Body.to_string body in
    put_key_value fe aid selector value
  | (`PATCH, Some(aid)) ->
    let%lwt value = Cohttp_lwt.Body.to_string body in
    put_key_value fe aid selector value
    (* put_delta_key_value fe aid selector value *)
  | (`DELETE, Some(_)) -> unsupported_operation meth selector
    (* remove_key_value fe aid selector *)
  | _ -> unsupported_operation meth selector


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
    execute_data_operation fe meth path headers body


let create cfg engine = 
  let _ = Logs_lwt.debug (fun m -> m "[FER] REST-FE preparing HTTP server") in
  let stop, stopper = Lwt.wait () in
  { cfg; engine; stop; stopper; request_counter=0L }

let start fe =
  let _ = Logs_lwt.debug (fun m -> m "[FER] REST-FE starting HTTP server on port %d" fe.cfg.port) in
  let callback _conn req body = execute_http_request fe req body in
  Server.create ~stop:fe.stop ~mode:(`TCP (`Port fe.cfg.port)) (Server.make ~callback ())


let stop fe =
  let _ = Logs_lwt.debug (fun m -> m "[FER] REST-FE stopping HTTP server") in
  Lwt.wakeup_later fe.stopper ()
