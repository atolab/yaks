open Apero
open Yaks_core
open Cohttp
open Cohttp_lwt
open LwtM.InfixM


module Make (YEngine : Yaks_engine.Engine.S) = struct 

  module Str = Re.Str

  module Server = Cohttp_lwt.Make_server(Cohttp_lwt_unix.IO)

  type config = { id : FeId.t; port : int }

  module TokenMap = Map.Make(String)

  type t = 
    { cfg: config
    ; engine: YEngine.t 
    ; stop: unit Lwt.t
    ; stopper: unit Lwt.u
    ; mutable request_counter: int64
    }


  (**********************************)
  (*      helpers functions         *)
  (**********************************)

  let http_header_eval = "is.yaks.eval"

  let query_to_string query =
    List.map (fun (n,v) -> Printf.sprintf "%s=%s" n (String.concat "," v)) query
    |> String.concat "&"


  (**********************************)
  (*        Error replies           *)
  (**********************************)

  let empty_path =
    Server.respond_string ~status:`Bad_request ~body:(
      "Welcome to YAKS REST API\n\n"^
      "Usage:\n\n"^
      "GET /a/b/c\n"^
      "   => get all the key/value for key '/a/b/c'\n"^
      "GET /@/local/**\n"^
      "   => get all keys/values from Admin Space\n"
    )
      ()

  (* let unsupported_uri path =
    Server.respond_error ~status:`Bad_request ~body:("No operation available on path: "^path) () *)

  let unsupported_operation operation path =
    Server.respond_error ~status:`Bad_request ~body:("Operation "^(Code.string_of_method operation)^" not supported on path: "^path) ()

  let invalid_path p =
    Server.respond_error ~status:`Not_found ~body:("Invalid Path  \""^p) ()

  let invalid_selector s =
     Server.respond_error ~status:`Not_found ~body:("Invalid Selector  \""^s) ()

  (* let no_matching_key selector =
    Server.respond_error ~status:`Not_found ~body:("No key found matching selector: "^selector) () *)

  (* let bad_request s =
     Server.respond_error ~status:`Bad_request ~body:("Invalid Request  \""^s) () *)

  let internal_error msg =
    Server.respond_error ~status:`Internal_server_error ~body:msg () 

  let ex_to_response ex =
    match ex with
   | YException e -> internal_error (show_yerror e)
   | ex -> internal_error (Printexc.to_string ex)



  (**********************************)
  (*      Key/Value operations      *)
  (**********************************)
  let json_string_of_key_values (kvs : (Path.t * Value.t) list) =
    let to_json v = match Value.transcode v Value.Json_Encoding with
     | Ok v' -> v'
     | _ -> v 
    in
    kvs
    |> List.map (fun (key, value) -> Printf.sprintf "\"%s\" : %s" (Path.to_string key)  (Value.to_string @@ to_json value))
    |> String.concat ",\n"
    |> Printf.sprintf "{%s}"

  let get fe clientid selector =
    let%lwt _ = Logs_lwt.debug (fun m -> m "[FER] get %s %s" (ClientId.to_string clientid) (Selector.to_string selector)) in
    YEngine.get fe.engine clientid selector
    >>= fun (kvs) -> 
    let headers = Header.init () |> fun h -> Header.add h "Access-Control-Allow-Origin" "*" in 
    Server.respond_string ~headers ~status:`OK ~body:(json_string_of_key_values kvs) ()    


  let put fe clientid path value =
    let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   put %s %s\n%s" (ClientId.to_string clientid) (Path.to_string path) (Value.to_string value)) in
    Lwt.try_bind 
      (fun () -> 
         let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   put calling YEngine.put") in
         YEngine.put fe.engine clientid path value) 
      (fun () -> Server.respond_string ~status:`No_content ~body:"" ())
      (fun ex -> ex_to_response ex)


  let update fe clientid path delta =  
    let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   update %s %s\n%s" (ClientId.to_string clientid) (Path.to_string path) (Value.to_string delta)) in  
    Lwt.try_bind 
      (fun () -> 
         let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   put_delta calling YEngine.put_delta") in
         YEngine.update fe.engine clientid path delta) 
      (fun () -> Server.respond_string ~status:`No_content ~body:"" ())
      (fun ex -> ex_to_response ex)

  let eval fe clientid selector =
    let%lwt _ = Logs_lwt.debug (fun m -> m "[FER] eval %s %s" (ClientId.to_string clientid) (Selector.to_string selector)) in
    YEngine.eval fe.engine clientid selector
    >>= fun (kvs) ->
    Server.respond_string ~status:`OK ~body:(json_string_of_key_values kvs) ()


  let remove fe clientid path =
    let%lwt _ = Logs_lwt.debug (fun m -> m "[FER]   remove %s %s" (ClientId.to_string clientid) (Path.to_string path)) in
    YEngine.remove fe.engine clientid path
    >>= fun () -> 
    Server.respond_string ~status:`No_content ~body:"" ()    


  (* Waiting for client identification over REST, we use a unique session id for all clients *)
  let default_session_id = SessionId.of_string "0"

  let init_default_session fe =
    let (clientid:ClientId.t) = { feid = fe.cfg.id; sid = default_session_id } in
    YEngine.login fe.engine clientid Properties.empty

  let close_default_session fe =
    let (clientid:ClientId.t) = { feid = fe.cfg.id; sid = default_session_id } in
    YEngine.logout fe.engine clientid

  (**********************************)
  (*   HTTP requests dispatching    *)
  (**********************************)
  let execute_http_request fe req body =
    (* let open Lwt in *)
    let meth = req |> Request.meth in
    let uri = req |> Request.uri in
    let path = uri |> Uri.path  in
    let query = uri |> Uri.query in
    let headers = req |> Request.headers in
    let%lwt _ = Logs_lwt.debug (fun m -> m "[FER] HTTP req: %s %s?%s with headers: %a" 
                                   (Code.string_of_method meth) path (query_to_string query)
                                   Header.pp_hum headers)
    in
    let (clientid:ClientId.t) = { feid = fe.cfg.id; sid = default_session_id } in
    try%lwt (
      if path = "/" then
        empty_path
      else
        match meth with
        | `GET ->
          (match Selector.of_string_opt (path^"?"^((query_to_string query))) with
          | Some selector ->
              (match Header.get headers http_header_eval with
              | Some _ -> eval fe clientid selector
              | None -> get fe clientid selector)
          | None -> invalid_selector path)
        | `PUT ->
          (match Path.of_string_opt path with
          | Some path ->
            let%lwt value = Cohttp_lwt.Body.to_string body in
            put fe clientid path (Value.JSonValue value)
          | None -> invalid_path path)
        | `PATCH ->
          (match Path.of_string_opt path with
          | Some path ->
            let%lwt value = Cohttp_lwt.Body.to_string body in
            update fe clientid path (Value.JSonValue value)
          | None -> invalid_path path)
        | `DELETE ->
          (match Path.of_string_opt path with
          | Some path -> remove fe clientid path
          | None -> invalid_path path)
        | _ -> unsupported_operation meth path
    ) with
    | YException e as exn -> 
      Logs_lwt.err (fun m -> m "Exception %s raised:\n%s" (show_yerror e) (Printexc.get_backtrace ())) >>= fun _ -> raise exn
    | exn ->
      Logs_lwt.err (fun m -> m "Exception %s raised:\n%s" (Printexc.to_string exn) (Printexc.get_backtrace ())) >>= fun _ -> raise exn


  let create cfg engine = 
    let _ = Logs_lwt.debug (fun m -> m "[FER] REST-FE preparing HTTP server") in
    let stop, stopper = Lwt.wait () in
    { cfg; engine; stop; stopper; request_counter=0L }

  let start fe =
    let _ = Logs_lwt.debug (fun m -> m "[FER] REST-FE starting HTTP server on port %d" fe.cfg.port) in
    let callback _conn req body = execute_http_request fe req body in
    (* let u = Cohttp_lwt_unix.Server.create ~stop:fe.stop  ~mode:(`TCP (`Port fe.cfg.port)) (Cohttp_lwt_unix.Server.make ~callback ()) in u *)
    let s = Server.make ~callback () in 
    let tcp = `TCP (`Port fe.cfg.port) in
    init_default_session fe >>= fun () ->
    Conduit_lwt_unix.serve ~ctx:Conduit_lwt_unix.default_ctx ~mode:tcp (Server.callback s)

  let stop fe =
    let _ = Logs_lwt.debug (fun m -> m "[FER] REST-FE stopping HTTP server") in
    close_default_session fe >|= fun () ->
    Lwt.wakeup_later fe.stopper ()

end