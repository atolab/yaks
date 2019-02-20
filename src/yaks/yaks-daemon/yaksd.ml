open Apero
open Apero.LwtM.InfixM
open Yaks_core
open Cmdliner

let listen_address = Unix.inet_addr_any
let backlog = 10
let max_buf_len = 64 * 1024

let yaks_id = Arg.(value & opt string "" & info ["y"; "yaks-id"] ~docv:"UUID|STRING"
                             ~doc:("If set, use this string to generate the Yaks unique identifier. If the string has UUID format, Yaks just uses it as identifier. "^
                                   "Otherwise, Yaks uses this string to generate an UUID in a determinstic way (i.e. a same string always generates the same UUID.
                                   If this option is not set, Yaks generates a random UUID."))
let without_storage = Arg.(value & flag & info ["w"; "without-storage"] ~docv:"true|false"
                             ~doc:"If true, disable the creation at startup of a default memory storage with '/**' as selector")
let http_port = Arg.(value & opt int 8000 & info ["h"; "http-port"] ~docv:"PORT"
                       ~doc:"HTTP port used by the REST front-end")
let sock_port = Arg.(value & opt int 7887 & info ["s"; "sock-port"] ~docv:"PORT"
                       ~doc:"TCP port used by the Socket front-end")

let wsock_port = Arg.(value & opt int 7888 & info ["x"; "wsock-port"] ~docv:"PORT"
                       ~doc:"Port used by the WebSocket front-end")

let sql_url = Arg.(value & opt string "" & info ["u"; "sql-url"] ~docv:"URL"
                       ~doc:"URL of the database used by the SQL backend")

let zenoh_locator = Arg.(value & opt string "none" & info ["z"; "zenoh"] ~docv:"zenoh locator or none"
                       ~doc:"A locator for the zenoh service, none for standalone YAKS instances")


let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()


module YEngine = Yaks_core.Engine
 

let (local_client_id:ClientId.t) = { feid = FeId.of_string "local"; sid = SessionId.of_string "daemon"}


let add_mem_be engine =
  YEngine.add_backend_TMP engine @@
    Yaks_be_mm.MainMemoryBEF.make (BeId.of_string "Memory") Properties.empty

let add_sql_be engine sql_url =
  if String.length sql_url > 0 then
    let sql_props = Properties.singleton Be_sql_properties.Key.url sql_url in
    YEngine.add_backend_TMP engine @@
      Yaks_be_sql.SQLBEF.make (BeId.of_string "SQL") sql_props
  else Lwt.return_unit


let add_default_storage engine without_storage =
  if not without_storage then 
    let path = "/@/local/backend/Memory/storage/default" in
    let props = Properties.singleton "selector" "/**" in
    YEngine.put engine local_client_id (Path.of_string path) (Value.PropertiesValue props)
  else Lwt.return_unit

let add_rest_fe engine http_port =
  let module YRestFE = Yaks_fe_rest.Make (YEngine) in 
  let restfe_cfg = YRestFE.{ id = FeId.of_string "REST"; port = http_port } in
  let restfe = YRestFE.create restfe_cfg  engine in
  YEngine.add_frontend_TMP engine "REST" @@ Properties.singleton "port" (string_of_int http_port) >>= fun _ ->
  YRestFE.start restfe

let add_socket_fe engine sock_port =
  let module YSockFE = Yaks_fe_sock.Make (YEngine) in
  let socket_addr = "tcp/0.0.0.0:"^(string_of_int sock_port) in
  let socket_cfg = YSockFE.Config.make (Apero.Option.get @@ Apero_net.TcpLocator.of_string socket_addr) in 
  let sockfe = YSockFE.create (FeId.of_string "TCP") socket_cfg engine in 
  YEngine.add_frontend_TMP engine "TCP" @@ Properties.singleton "port" (string_of_int sock_port) >>= fun _ ->
  YSockFE.start sockfe

let add_websock_fe engine wsock_port = 
  let module WSockFE = Yaks_fe_wsock.Make (YEngine) in 
  let wsock_addr = "ws/0.0.0.0:"^(string_of_int wsock_port) in  
  let wsock_cfg = WSockFE.Config.make (Apero.Option.get @@ Apero_net.WebSockLocator.of_string wsock_addr) in 
  let wsfe = WSockFE.create (FeId.of_string "WSOCK") wsock_cfg engine in
  YEngine.add_frontend_TMP engine "WSOCK" @@ Properties.singleton "port" (string_of_int wsock_port) >>= fun _ ->
  WSockFE.start wsfe

let run_yaksd yid without_storage http_port sock_port wsock_port sql_url zenoh_locator = 
  try%lwt
    let%lwt zenoh = 
      if zenoh_locator = "none" then Lwt.return None
      else let%lwt z = Zenoh.zopen zenoh_locator in Lwt.return (Some z)
    in
    let engine = YEngine.make ?id:(if String.length yid > 0 then Some yid else None) zenoh in
    let mem_be = add_mem_be engine in
    let sql_be = add_sql_be engine sql_url in
    let def_store = add_default_storage engine without_storage >>= fun _ -> Lwt.return_unit in
    let rest_fe = add_rest_fe engine http_port in
    let sock_fe = add_socket_fe engine sock_port in
    let wsock_fe = add_websock_fe engine wsock_port in
    Lwt.join [mem_be; sql_be; def_store; rest_fe; sock_fe; wsock_fe]
  with 
  | YException e  -> 
    Logs_lwt.err (fun m -> m "Exception %s raised:\n%s" (show_yerror e) (Printexc.get_backtrace ())) >> Lwt.return_unit
  | exn -> 
    Logs_lwt.err (fun m -> m "Exception %s raised:\n%s" (Printexc.to_string exn) (Printexc.get_backtrace ())) >> Lwt.return_unit


let run yid without_storage http_port sock_port wsock_port sql_url zenoh_locator style_renderer level = 
  setup_log style_renderer level; 
  Lwt_main.run @@ run_yaksd yid without_storage http_port sock_port wsock_port sql_url zenoh_locator 


let () =
  Printexc.record_backtrace true;
  let env = Arg.env_var "YAKSD_VERBOSITY" in
  let _ = Term.(eval (const run $ yaks_id $ without_storage $ http_port $ sock_port $ wsock_port $ sql_url $ zenoh_locator $ Fmt_cli.style_renderer () $ Logs_cli.level ~env (), Term.info "Yaks daemon")) in  ()
