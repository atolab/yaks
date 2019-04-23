open Apero
open Apero.LwtM.InfixM
open Yaks_core
open Cmdliner

let listen_address = Unix.inet_addr_any
let backlog = 10
let max_buf_len = 64 * 1024


let config_file = Arg.(value & opt (some ~none:"default configuration" string) None & info ["c"; "config"] ~docv:"FILENAME"
                             ~doc:("A JSON configuration file to load. The expected format is exactly the same than the JSON element for a Yaks service in the admin space "^
                                   "(for an example, do 'curl http://localhost:8000/@/local' on a Yaks service running on your host). Note that only the 'configuration' elements "^
                                   "will be considered; the other elements will be ignored.\n"^
                                   "If not specified, the default configuration is the following:\n"^
                                   (Yaks_config_v.create_config () |> Yaks_config_j.string_of_config |> Yojson.Safe.prettify)))

let yaks_id = Arg.(value & opt (some ~none:"a random UUID" string) None & info ["y"; "yaks-id"] ~docv:"UUID|STRING"
                             ~doc:("If set, use this string to generate the Yaks unique identifier. If the string has UUID format, Yaks just uses it as identifier. "^
                                   "Otherwise, Yaks uses this string to generate an UUID in a determinstic way (i.e. a same string always generates the same UUID.
                                   If this option is not set, Yaks generates a random UUID."))
let without_storage = Arg.(value & flag & info ["w"; "without-storage"] ~docv:"true|false"
                             ~doc:"If true, disable the creation at startup of a default memory storage with '/**' as selector")
let http_port = Arg.(value & opt (some ~none:"the port specified in the configuration (file or default)" int) None & info ["h"; "http-port"] ~docv:"PORT"
                       ~doc:"HTTP port used by the REST frontend")
let sock_port = Arg.(value & opt (some ~none:"the port specified in the configuration (file or default)" int) None & info ["s"; "sock-port"] ~docv:"PORT"
                       ~doc:"TCP port used by the Socket frontend")

let wsock_port = Arg.(value & opt (some ~none:"the port specified in the configuration (file or default)" int) None & info ["x"; "wsock-port"] ~docv:"PORT"
                       ~doc:"Port used by the WebSocket frontend")

let sql_url = Arg.(value & opt (some ~none:"the url specified in the configuration (file or default)" string) None & info ["u"; "sql-url"] ~docv:"URL"
                       ~doc:"URL of the database used by the SQL backend")

let zenoh_locator = Arg.(value & opt (some ~none:"the locator specified in the configuration (file or default)" string) None & info ["z"; "zenoh"] ~docv:"zenoh locator or none"
                       ~doc:"A locator for the zenoh service. If not specified YAKS is standalone.")


let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()


module YEngine = Yaks_core.Engine
 

let (local_client_id:ClientId.t) = { feid = FeId.of_string "local"; sid = SessionId.of_string "daemon"}


let add_mem_be engine (config:Yaks_config_t.config) =
  match List.find_opt (fun (beid, _) -> beid == "Memory") config.backends with
  | Some (_, be) -> YEngine.add_backend_TMP engine @@ Yaks_be_mm.MainMemoryBEF.make (BeId.of_string "Memory") be.configuration
  | None -> Lwt.return_unit

let add_sql_be engine (config:Yaks_config_t.config) =
  match List.find_opt (fun (beid, _) -> beid == "Memory") config.backends with
  | Some (_, be) -> YEngine.add_backend_TMP engine @@ Yaks_be_sql.SQLBEF.make (BeId.of_string "SQL") be.configuration
  | None -> Lwt.return_unit


let add_storages engine (config:Yaks_config_t.config) =
  List.iter (fun (beid, (be:Yaks_config_t.backend)) ->
    List.iter (fun (stid, (st:Yaks_config_t.storage)) ->
      let path = "/@/local/backend/"^beid^"/storage/"^stid in
      Lwt.async (fun () -> YEngine.put engine local_client_id (Path.of_string path) (Value.PropertiesValue st.configuration))
    ) be.storages
  ) config.backends

(* let add_default_storage engine without_storage =
  if not without_storage then 
    let path = "/@/local/backend/Memory/storage/default" in
    let props = Properties.singleton "selector" "/**" in
    YEngine.put engine local_client_id (Path.of_string path) (Value.PropertiesValue props)
  else Lwt.return_unit *)

let add_rest_fe engine (config:Yaks_config_t.config) =
  match List.find_opt (fun (feid, _) -> feid == "REST") config.frontends with
  | Some (_, fe) ->
    let port = int_of_string @@ Properties.find "port" fe.configuration in
    let module YRestFE = Yaks_fe_rest.Make (YEngine) in
    let restfe_cfg = YRestFE.{ id = FeId.of_string "REST"; port } in
    let restfe = YRestFE.create restfe_cfg  engine in
    YEngine.add_frontend_TMP engine "REST" fe.configuration >>= fun _ ->
    YRestFE.start restfe
  | None -> Lwt.return_unit

let add_socket_fe engine (config:Yaks_config_t.config) =
  match List.find_opt (fun (feid, _) -> feid == "TCP") config.frontends with
  | Some (_, fe) ->
    let socket_addr = "tcp/0.0.0.0:"^(Properties.find "port" fe.configuration) in
    let module YSockFE = Yaks_fe_sock.Make (YEngine) in
    let socket_cfg = YSockFE.Config.make (Apero.Option.get @@ Apero_net.TcpLocator.of_string socket_addr) in
    let sockfe = YSockFE.create (FeId.of_string "TCP") socket_cfg engine in
    YEngine.add_frontend_TMP engine "TCP" fe.configuration >>= fun _ ->
    YSockFE.start sockfe
  | None -> Lwt.return_unit

let add_websock_fe engine (config:Yaks_config_t.config) = 
  match List.find_opt (fun (feid, _) -> feid == "WSOCK") config.frontends with
  | Some (_, fe) ->
    let wsock_addr = "ws/0.0.0.0:"^(Properties.find "port" fe.configuration) in
    let module WSockFE = Yaks_fe_wsock.Make (YEngine) in
    let wsock_cfg = WSockFE.Config.make (Apero.Option.get @@ Apero_net.WebSockLocator.of_string wsock_addr) in
    let wsfe = WSockFE.create (FeId.of_string "WSOCK") wsock_cfg engine in
    YEngine.add_frontend_TMP engine "WSOCK" fe.configuration >>= fun _ ->
    WSockFE.start wsfe
  | None -> Lwt.return_unit


let config_to_kvs (config:Yaks_config_t.config) =
  let kvs = [] in
  (* add frontends at the end of kvs list, as they must be added after backends and storages creation *)
  let kvs = List.fold_left (fun kvs (feid, (fe:Yaks_config_t.frontend)) ->
    ("/@/local/frontends/"^feid^"/configuration", fe.configuration)::kvs
  ) kvs config.frontends
  in
  (* then add storages, as they must be added after backends creation *)
  let kvs = List.fold_left (fun kvs (beid, (be:Yaks_config_t.backend)) ->
    List.fold_left (fun kvs (stid, (st:Yaks_config_t.storage)) -> ("/@/local/backends/"^beid^"/storages/"^stid^"/configuration", st.configuration)::kvs) kvs be.storages
  ) kvs config.backends
  in
  (* then add backends *)
  let kvs = List.fold_left (fun kvs (beid, (be:Yaks_config_t.backend)) ->
    ("/@/local/backends/"^beid^"/configuration", be.configuration)::kvs
  ) kvs config.backends
  in
  (* finally add zenoh *)
  let kvs = match config.transport with
    | Some t -> ("/@/local/transport/zenoh", t.zenoh.configuration)::kvs
    | None -> kvs
  in
  kvs

let run_yaksd yid (config:Yaks_config_t.config) =
  let _ = Logs.debug (fun m -> m "Starting Yaks with configuration:\n%s" (Yaks_config_j.string_of_config config |> Yojson.Safe.prettify)) in
  try%lwt
    let%lwt zenoh = match config.transport with
      | Some t -> Yaks_zenoh_utils.zopen t.zenoh.configuration >>= Lwt.return_some
      | None -> Lwt.return_none
    in
    let engine = YEngine.make ?id:yid zenoh in
    let%lwt () = add_mem_be engine config in
    let%lwt () = add_sql_be engine config in
    let () = add_storages engine config in
    let rest_fe = add_rest_fe engine config in
    let sock_fe = add_socket_fe engine config in
    let wsock_fe = add_websock_fe engine config in
    Lwt.join [rest_fe; sock_fe; wsock_fe]
  with
  | YException e  -> 
    Logs_lwt.err (fun m -> m "Exception %s raised:\n%s" (show_yerror e) (Printexc.get_backtrace ())) >> Lwt.return_unit
  | exn -> 
    Logs_lwt.err (fun m -> m "Exception %s raised:\n%s" (Printexc.to_string exn) (Printexc.get_backtrace ())) >> Lwt.return_unit

  (* try%lwt
    let config = load_config config_file without_storage http_port sock_port wsock_port sql_url zenoh_locator in

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
    Lwt.return_unit *)



let load_config config_file zenoh_locator sock_port http_port wsock_port sql_url without_storage =
  let open Yaks_config_t in
  let config = match config_file with
      | Some file -> 
        let c = Yaks_config_j.read_config (Yojson.init_lexer ~fname:file ()) (Lexing.from_channel (open_in file)) in
        let _ = Logs.debug (fun m -> m "Loaded configuration from %s:\n%s" file (Yaks_config_j.string_of_config c |> Yojson.Safe.prettify)) in
        c
      | None -> Yaks_config_v.create_config ()
  in
  let _ = Logs.debug (fun m -> m "Configuration before command line options:\n%s" (Yaks_config_j.string_of_config config |> Yojson.Safe.prettify)) in
  let config = match zenoh_locator with
    | Some locator -> {config with transport = Some {zenoh={configuration=Properties.singleton "peer" locator }} }
    | None -> config
  in
  let config = match sock_port , List.find_opt (fun (feid, _) -> feid == "TCP") config.frontends with
    | Some port , Some (_, fe) ->
      let frontends' = List.filter (fun (feid, _) -> feid <> "TCP") config.frontends in
      let (tcp_fe:(string*frontend)) = ("TCP", {configuration=Properties.add "port" (string_of_int port) fe.configuration}) in
      {config with frontends = tcp_fe::frontends' }
    | Some port , None ->
      let (tcp_fe:(string*frontend)) = ("TCP", {configuration=Properties.singleton "port" (string_of_int port)}) in
      {config with frontends = tcp_fe::config.frontends }
    | None , _ -> config
  in
  let config = match http_port , List.find_opt (fun (feid, _) -> feid == "REST") config.frontends with
    | Some port , Some (_, fe) ->
      let frontends' = List.filter (fun (feid, _) -> feid <> "REST") config.frontends in
      let (rest_fe:(string*frontend)) = ("REST", {configuration=Properties.add "port" (string_of_int port) fe.configuration}) in
      {config with frontends = rest_fe::frontends' }
    | Some port , None ->
      let (rest_fe:(string*frontend)) = ("REST", {configuration=Properties.singleton "port" (string_of_int port)}) in
      {config with frontends = rest_fe::config.frontends }
    | None , _ -> config
  in
  let config = match wsock_port , List.find_opt (fun (feid, _) -> feid == "WSOCK") config.frontends with
    | Some port , Some (_, fe) ->
      let frontends' = List.filter (fun (feid, _) -> feid <> "WSOCK") config.frontends in
      let (ws_fe:(string*frontend)) = ("WSOCK", {configuration=Properties.add "port" (string_of_int port) fe.configuration}) in
      {config with frontends = ws_fe::frontends' }
    | Some port , None ->
      let (ws_fe:(string*frontend)) = ("WSOCK", {configuration=Properties.singleton "port" (string_of_int port)}) in
      {config with frontends = ws_fe::config.frontends }
    | None , _ -> config
  in
  let config = match sql_url , List.find_opt (fun (beid, _) -> beid == "SQL") config.backends with
    | Some url , Some (_, be) ->
      let backends' = List.filter (fun (beid, _) -> beid <> "SQL") config.backends in
      let (sql_be:(string*backend)) = ("SQL", {configuration=Properties.add "is.yaks.backend.sql.url" url be.configuration; storages=be.storages}) in
      {config with backends = sql_be::backends' }
    | Some url , None ->
      let (sql_be:(string*backend)) = ("SQL", {configuration=Properties.singleton "is.yaks.backend.sql.url" url; storages=[]}) in
      {config with backends = sql_be::config.backends }
    | None , _ -> config
  in
  let config = match without_storage , List.find_opt (fun (beid, _) -> beid == "Memory") config.backends with
    | false , Some (_, be) ->
      let backends' = List.filter (fun (beid, _) -> beid <> "Memory") config.backends in
      let (mem_be:(string*backend)) = ("Memory", {configuration=be.configuration; storages=[("default", {configuration=Properties.singleton "selector" "/**"})]}) in
      {config with backends = mem_be::backends' }
    | false , None ->
      let (mem_be:(string*backend)) = ("Memory", {configuration=Properties.singleton "kind" "memory"; storages=[("default", {configuration=Properties.singleton "selector" "/**"})]}) in
      {config with backends = mem_be::config.backends }
    | true , _  -> config

  in
  config


let run config_file yid without_storage http_port sock_port wsock_port sql_url zenoh_locator style_renderer level = 
  setup_log style_renderer level;
  (* Note: by default the Lwt.async_exception_hook do "exit 2" when an exception is raised in a canceled Lwt task.
     We rather force it to log and ignore the exception to avoid crashes (as it occurs randomly within cohttp at connection closure).  *)
  Lwt.async_exception_hook := (fun exn ->
    ignore @@ Logs.debug (fun m -> m "Exception caught in Lwt.async_exception_hook: %s\n%s" (Printexc.to_string exn) (Printexc.get_backtrace ())));
  let config = load_config config_file zenoh_locator sock_port http_port wsock_port sql_url without_storage in
  Lwt_main.run @@ run_yaksd yid config


let () =
  Printexc.record_backtrace true;
  Lwt_engine.set (new Lwt_engine.libev ());
  let env = Arg.env_var "YAKSD_VERBOSITY" in
  let _ = Term.(eval (const run $ config_file $ yaks_id $ without_storage $ http_port $ sock_port $ wsock_port $ sql_url $ zenoh_locator $ Fmt_cli.style_renderer () $ Logs_cli.level ~env (), Term.info "Yaks daemon")) in  ()
