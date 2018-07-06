open Apero
open Yaks_core
open Yaks_event
open Cohttp
open Cohttp_lwt_unix


type config = { port : int; stream_len : int }

type t = {
  cfg: config;
  request_sink: event_sink;
  reply_sink: event_sink;
  stop: unit Lwt.t;
  stopper: unit Lwt.u;
  mutable request_counter: int64;
}

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
  let open Acommon.OptionM.InfixM in 
  List.find_opt (fun (n, v) -> n = name) query >>= fun (_, v) -> Some(v)

let query_to_string query =
  List.map (fun (n,v) -> n^"="^(String.concat "," v)) query |> String.concat "&"


let string_of_property : property -> string =
  fun p ->p.key^"="^p.value

let string_of_properties p =
  String.concat ""
    [ "["; List.map string_of_property p |> String.concat ","; "]"]


let string_of_entity e = match e with
  | Access{path; cache_size} -> "Acc("^path^")"
  | Storage{path; properties}    -> "Str("^path^")"
  | Subscriber{access_id; selector; push} -> "Sub("^selector^")"

let string_of_entity_id eid = match eid with
  | Yaks -> "yaks"
  | AccessId s -> s
  | StorageId s -> s
  | SubscriberId i -> Int64.to_string i
  | Auto -> "auto"

let string_of_message msg = 
  match msg with
  | Create{cid; entity; entity_id} ->
    "#"^(Int64.to_string cid)^" Create("^(string_of_entity_id entity_id)^":"^(string_of_entity entity)^")"
  | Dispose{cid; entity_id} ->
    "#"^(Int64.to_string cid)^" Dispose("^(string_of_entity_id entity_id)^")"
  | Get{cid; entity_id; key} ->
    "#"^(Int64.to_string cid)^" Get("^key^")"
  | Put{cid; access_id; key; value} ->
    "#"^(Int64.to_string cid)^" Put("^key^", "^value^")"
  | Patch{cid; access_id; key; value} ->
    "#"^(Int64.to_string cid)^" Patch("^key^", "^value^")"
  | Remove{cid; access_id; key} ->
    "#"^(Int64.to_string cid)^" Get("^key^")"
  | Notify{cid; sid; values} ->
    "#"^(Int64.to_string cid)^" Notify("^(string_of_entity_id sid)^")"
  | Values{cid; values} ->
    "#"^(Int64.to_string cid)^" Values(...)"
  | Error{cid; reason} ->
    "#"^(Int64.to_string cid)^" Error("^(string_of_int reason)^")"
  | Ok{cid; entity_id} ->
    "#"^(Int64.to_string cid)^" Ok("^(string_of_entity_id entity_id)^")"

let string_of_values values =
  String.concat "" [
    "{\n";
    values |> List.map (fun {key; value} ->
        String.concat "" ["{ "; key; " , "; Lwt_bytes.to_string value;" }\n";])
    |> String.concat "\n";
    "}\n";
  ]

let properties_of_query =
  List.map (fun (k, v):property -> {key=k; value=(String.concat "," v)}) 


let push_to_engine sink msg on_reply =
  let _ = Logs_lwt.debug (fun m -> m "   send to engine %s" (string_of_message
                                                               msg)) in
  let (promise, resolver) = Lwt.wait () in
  let event_handler = fun reply ->
    let _ = Logs_lwt.debug (fun m -> m "   recv from engine %s" (string_of_message
                                                                   reply)) in
    on_reply reply |> Lwt.wakeup_later resolver |> Lwt.return
  in
  let open Lwt in
  EventStream.Sink.push (EventWithHandler (msg, event_handler)) sink >>= fun _ ->
  promise >>= fun http_reply ->
  http_reply




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

let create_access fe ?id path cache_size =
  let _ = Logs_lwt.debug (fun m -> m "  create_access %s %s %Ld" (OptionM.get ~if_none:"?" id) path cache_size) in
  let msg = Create { 
      cid = next_request_counter fe;
      entity = Access {path; cache_size};
      entity_id = match id with None -> Auto | Some(i) -> AccessId(i)
    }
  in
  let on_reply = fun reply -> match reply with
    | Ok {cid; entity_id=AccessId(aid)} ->
      let headers = Header.add_list (Header.init()) 
          [("Location",match id with None -> aid | Some(_) -> ".");
           ("Set-Cookie",cookie_name_access_id^"="^aid)]
      in
      Server.respond_string ~status:`Created ~headers ~body:"" ()
    | Error {cid; reason=507} ->
      insufficient_storage cache_size
    | _ ->
      unexpected_reply reply
  in
  push_to_engine fe.request_sink msg on_reply

let get_access ?id fe =
  let _ = Logs_lwt.debug (fun m -> m "  get_access %s" (OptionM.get ~if_none:"?" id)) in
  let msg = Get { 
      cid = next_request_counter fe;
      entity_id = Yaks;
      key = "access"^(OptionM.get ~if_none:"" id);
      encoding = Some(`Json)
    }
  in
  let on_reply = fun reply -> match reply with
    | Values {cid; encoding=`Json; values} ->
      Server.respond_string ~status:`OK ~body:(string_of_values values) ()
    | Error {cid; reason=404} ->
      access_not_found (OptionM.get ~if_none:"NO_ID!!!" id)
    | _ ->
      unexpected_reply reply
  in
  push_to_engine fe.request_sink msg on_reply


let dispose_access fe id =
  let _ = Logs_lwt.debug (fun m -> m "  dispose_access %s" id) in
  let msg = Dispose { 
      cid = next_request_counter fe;
      entity_id = AccessId(id)
    }
  in
  let on_reply = fun reply -> match reply with
    | Ok{cid; entity_id=AccessId(xid)} ->
      Server.respond_string ~status:`No_content ~body:"" ()
    | Error{cid; reason=404} ->
      access_not_found id
    | _ ->
      unexpected_reply reply
  in
  push_to_engine fe.request_sink msg on_reply

let create_storage fe ?id path properties =
  let _ = Logs_lwt.debug (fun m -> m "  create_storage %s %s" (OptionM.get ~if_none:"?" id) path) in
  let msg = Create { 
      cid = next_request_counter fe;
      entity = Storage {path; properties};
      entity_id = match id with None -> Auto | Some(i) -> StorageId(i)
    }
  in
  let on_reply = fun reply -> match reply with
    | Ok {cid; entity_id=StorageId(sid)} ->
      let headers = Header.add_list (Header.init())
          [("Location", match id with None -> sid | Some(_) -> ".");
           ("Set-Cookie",cookie_name_storage_id^"="^sid)] in
      Server.respond_string ~status:`Created ~headers ~body:"" ()
    | Error {cid; reason=501} ->
      unkown_storage_type properties
    | _ ->
      unexpected_reply reply
  in
  push_to_engine fe.request_sink msg on_reply

let get_storage ?id fe =
  let _ = Logs_lwt.debug (fun m -> m "  get_storage %s" (OptionM.get ~if_none:"?" id)) in
  let msg = Get { 
      cid = next_request_counter fe;
      entity_id = Yaks;
      key = "storages"^(OptionM.get ~if_none:"" id);
      encoding = Some(`Json)
    }
  in
  let on_reply = fun reply -> match reply with
    | Values {cid; encoding=`Json; values} ->
      Server.respond_string ~status:`OK ~body:(string_of_values values) ()
    | Error {cid; reason=404} ->
      storage_not_found (OptionM.get ~if_none:"NO_ID!!!" id)
    | _ ->
      unexpected_reply reply
  in
  push_to_engine fe.request_sink msg on_reply

let dispose_storage fe id =
  let _ = Logs_lwt.debug (fun m -> m "  dispose_storage %s" id) in
  let msg = Dispose { 
      cid = next_request_counter fe;
      entity_id = StorageId(id)
    }
  in
  let on_reply = fun reply -> match reply with
    | Ok {cid; entity_id=StorageId(sid)} ->
      Server.respond_string ~status:`No_content ~body:"" ()
    | Error {cid; reason=404} ->
      storage_not_found id
    | _ ->
      unexpected_reply reply
  in
  push_to_engine fe.request_sink msg on_reply

let subscribe fe access_id selector =
  let _ = Logs_lwt.debug (fun m -> m "  subscribe %s %s" access_id selector) in
  let msg = Create { 
      cid = next_request_counter fe;
      entity = Subscriber {access_id; selector; push=false};
      entity_id = Auto
    }
  in
  let on_reply = fun reply -> match reply with
    | Ok {cid; entity_id=SubscriberId(sid)} ->
      let headers = Header.add_list (Header.init()) [("Location",Int64.to_string sid);] in
      Server.respond_string ~status:`Created ~headers ~body:"" ()
    | Error {cid; reason=412} ->
      access_not_found access_id
    | _ ->
      unexpected_reply reply
  in
  push_to_engine fe.request_sink msg on_reply

let get_subscriptions fe access_id =
  let _ = Logs_lwt.debug (fun m -> m "  get_subscriptions %s" access_id) in
  let msg = Get { 
      cid = next_request_counter fe;
      entity_id = Yaks;
      key = "access/"^access_id^"/sub";
      encoding = Some(`Json)
    }
  in
  let on_reply = fun reply -> match reply with
    | Values {cid; encoding=`Json; values} ->
      Server.respond_string ~status:`OK ~body:(string_of_values values) ()
    | Error {cid; reason=404} ->
      access_not_found access_id
    | _ ->
      unexpected_reply reply
  in
  push_to_engine fe.request_sink msg on_reply


let unsubscribe fe access_id sub_id =
  let _ = Logs_lwt.debug (fun m -> m "  unsubscribe %s %Ld" access_id sub_id) in
  let msg = Dispose { 
      cid = next_request_counter fe;
      entity_id = SubscriberId(sub_id)
    }
  in
  let on_reply = fun reply -> match reply with
    | Ok {cid; entity_id=SubscriberId(sid)} ->
      Server.respond_string ~status:`No_content ~body:"" ()
    | Error {cid; reason=404} ->
      access_not_found access_id
    | _ ->
      unexpected_reply reply
  in
  push_to_engine fe.request_sink msg on_reply

(**********************************)
(*      Key/Value operations      *)
(**********************************)

let status_ok = `Ok

let get_key_value fe access_id selector =
  let _ = Logs_lwt.debug (fun m -> m "  get_key_value %s %s" access_id selector) in
  let msg = Get { 
      cid = next_request_counter fe;
      entity_id = AccessId(access_id);
      key = selector;
      encoding = None
    }
  in
  let on_reply = fun reply -> match reply with
    | Values {cid; values} ->
      Server.respond_string ~status:`OK ~body:(string_of_values values) ()
    | Error {cid; reason=404} ->
      no_matching_key selector
    | _ ->
      unexpected_reply reply
  in
  push_to_engine fe.request_sink msg on_reply

let put_key_value fe access_id selector value =
  let _ = Logs_lwt.debug (fun m -> m "  put_key_value %s %s" access_id selector) in
  let msg = Put { 
      cid = next_request_counter fe;
      access_id = AccessId(access_id);
      key = selector;
      value
    }
  in
  let on_reply = fun reply -> match reply with
    | Ok {cid; _} ->
      Server.respond_string ~status:`No_content ~body:"" ()
    | Error {cid; reason=404} ->
      no_matching_key selector
    | _ ->
      unexpected_reply reply
  in
  push_to_engine fe.request_sink msg on_reply

let put_delta_key_value fe access_id selector delta_value =
  let _ = Logs_lwt.debug (fun m -> m "  put_delta_key_value %s %s" access_id selector) in
  let msg = Patch { 
      cid = next_request_counter fe;
      access_id = AccessId(access_id);
      key = selector;
      value=delta_value
    }
  in
  let on_reply = fun reply -> match reply with
    | Ok {cid; _} ->
      Server.respond_string ~status:`No_content ~body:"" ()
    | Error {cid; reason=404} ->
      no_matching_key selector
    | _ ->
      unexpected_reply reply
  in
  push_to_engine fe.request_sink msg on_reply

let remove_key_value fe access_id selector =
  let _ = Logs_lwt.debug (fun m -> m "  put_delta_key_value %s %s" access_id selector) in
  let msg = Remove { 
      cid = next_request_counter fe;
      access_id = AccessId(access_id);
      key = selector;
    }
  in
  let on_reply = fun reply -> match reply with
    | Ok {cid; _} ->
      Server.respond_string ~status:`No_content ~body:"" ()
    | Error {cid; reason=404} ->
      no_matching_key selector
    | _ ->
      unexpected_reply reply
  in
  push_to_engine fe.request_sink msg on_reply





(**********************************)
(*   HTTP requests dispatching    *)
(**********************************)

let execute_control_operation fe meth path query headers body =
  match (meth, path) with
  (* POST /yaks/access ? path & cacheSize *)
  | (`POST, ["yaks"; "access"]) -> (
      let open OptionM.InfixM in
      let access_path =  query_get_opt query "path" >== List.hd in
      let cache_size = query_get_opt query "cacheSize" >== List.hd >>= Int64.of_string_opt in
      match (access_path, cache_size) with
      | (None, None) -> missing_query (["path:string"; "cacheSize:int"])
      | (None, _)    -> missing_query (["path:string"])
      | (_, None)    -> missing_query (["cacheSize:int"])
      | (Some(path), Some(cache_size)) -> create_access fe path cache_size
    )
  (* PUT /yaks/access/id ? path & cacheSize *)
  | (`PUT, ["yaks"; "access"; id]) -> (
      let open OptionM.InfixM in
      let access_path =  query_get_opt query "path" >== List.hd in
      let cache_size = query_get_opt query "cacheSize" >== List.hd >>= Int64.of_string_opt in
      match (access_path, cache_size) with
      | (None, None) -> missing_query (["path:string"; "cacheSize:int"])
      | (None, _)    -> missing_query (["path:string"])
      | (_, None)    -> missing_query (["cacheSize:int"])
      | (Some(path), Some(cache_size)) -> create_access fe path cache_size ~id
    )
  (* GET /yaks/access *)
  | (`GET, ["yaks"; "access"]) -> (
      get_access fe
    )
  (* GET /yaks/access/id *)
  | (`GET, ["yaks"; "access"; id]) -> (
      get_access fe ~id
    )
  (* Dispose /yaks/access/id *)
  | (`DELETE, ["yaks"; "access"; id]) -> (
      dispose_access fe id
    )
  (* POST /yaks/storages ? path & options... *)
  | (`POST, ["yaks"; "storages"]) -> (
      let open OptionM.InfixM in
      let storage_path =  query_get_opt query "path" >== List.hd in
      match storage_path with
      | None -> missing_query (["path:string"])
      | Some(path) -> 
        let properties = query |> List.filter (fun (n, _) -> n != "path") |> properties_of_query in
        create_storage fe path properties
    )
  (* PUT /yaks/storages/id ? path & options... *)
  | (`PUT, ["yaks"; "storages"; id]) -> (
      let open OptionM.InfixM in
      let storage_path =  query_get_opt query "path" >== List.hd in
      match storage_path with
      | None -> missing_query (["path:string"])
      | Some(path) -> 
        let properties = query |> List.filter (fun (n, _) -> n != "path") |> properties_of_query in
        create_storage fe path properties ~id
    )
  (* GET /yaks/storages *)
  | (`GET, ["yaks"; "storages"]) -> (
      get_storage fe
    )
  (* GET /yaks/storages/id *)
  | (`GET, ["yaks"; "storages"; id]) -> (
      get_storage fe ~id
    )
  (* Dispose /yaks/storages/id *)
  | (`DELETE, ["yaks"; "storages"; id]) -> (
      dispose_storage fe id
    )
  (* POST /yaks/access/id/subs *)
  | (`POST, ["yaks"; "access"; aid; "subs"]) -> (
      let open OptionM.InfixM in
      let selector =  query_get_opt query "selector" >== List.hd in
      match selector with
      | None -> missing_query (["selector:string"])
      | Some(selector) -> 
        subscribe fe aid selector
    )
  (* Otherwise... *)
  | (_, _) -> String.concat "/" path |> unsupported_uri


let execute_data_operation fe meth selector headers body =
  let access_id = Header.get headers cookie_name_access_id in
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
    put_delta_key_value fe aid selector value
  | (`DELETE, Some(aid)) ->
    remove_key_value fe aid selector
  | _ -> unsupported_operation meth selector



let execute_http_request fe req body =
  let open Lwt in
  let meth = req |> Request.meth in
  let uri = req |> Request.uri in
  let path = uri |> Uri.path  in
  let query = uri |> Uri.query in
  let headers = req |> Request.headers in
  let _ = Logs_lwt.debug (fun m -> m "HTTP req: %s %s?%s with headers: %s" (Code.string_of_method meth) path (query_to_string query) (Header.to_string headers))
  in
  if path = "/" then
    empty_path
  else if String.length path >= 5 && String.sub path 0 5 = yaks_control_uri_prefix then
    let normalized_path = path |> String.split_on_char '/' |> List.filter (fun s -> String.length s > 0) in
    execute_control_operation fe meth normalized_path query headers body
  else
    execute_data_operation fe meth (Uri.to_string uri) headers body


let create cfg request_sink = 
  let _ = Logs_lwt.debug (fun m -> m "Socket-FE-REST preparing HTTP server") in
  let reply_src, reply_sink = EventStream.create cfg.stream_len in
  let stop, stopper = Lwt.wait () in
  { cfg; request_sink; reply_sink; stop; stopper; request_counter=0L }

let start fe =
  let _ = Logs_lwt.debug (fun m -> m "Socket-FE-REST starting HTTP server on port %d" fe.cfg.port) in
  let callback _conn req body = execute_http_request fe req body in
  Server.create ~stop:fe.stop ~mode:(`TCP (`Port fe.cfg.port)) (Server.make ~callback ())


let stop fe =
  let _ = Logs_lwt.debug (fun m -> m "Socket-FE-REST stopping HTTP server") in
  Lwt.wakeup_later fe.stopper ()

