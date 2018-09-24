open Yaks_core
open Cmdliner
open Apero.LwtM.InfixM

let listen_address = Unix.inet_addr_any
let backlog = 10
let max_buf_len = 64 * 1024


let without_storage = Arg.(value & flag & info ["w"; "without-storage"] ~docv:"true|false"
                             ~doc:"If true, disable the creation at startup of a default memory storage with '//' as path")
let http_port = Arg.(value & opt int 8000 & info ["h"; "http-port"] ~docv:"PORT"
                       ~doc:"HTTP port used by the REST front-end")
let sock_port = Arg.(value & opt int 7887 & info ["s"; "sock-port"] ~docv:"PORT"
                       ~doc:"TCP port used by the Socket front-end")


let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()


module YEngine = Yaks_core.SEngine.Make (Apero.MVar_lwt)

let add_default_storage engine without_storage =
  let props = Property.Map.singleton Property.Backend.Key.kind Property.Backend.Value.memory in
  if not without_storage then 
    (* Some (YEngine.create_storage engine (Path.of_string "//") props) *)
    YEngine.create_storage engine (Path.of_string "//") props >|= Apero.Option.return
  else Lwt.return_none

let add_rest_fe engine http_port =
  let module YRestFE = Yaks_fe_rest.Make (YEngine) in 
  let restfe_cfg = YRestFE.{ port = http_port } in
  let restfe = YRestFE.create restfe_cfg  engine in 
  YRestFE.start restfe

let add_socket_fe engine sock_port =
  let module YSockFE = Yaks_fe_sock.Make (YEngine) in 
  let socket_addr = "tcp/0.0.0.0:"^(string_of_int sock_port) in
  let socket_cfg = YSockFE.Config.make (Apero.Option.get @@ Apero_net.TcpLocator.of_string socket_addr) in 
  let sockfe = YSockFE.create socket_cfg engine in 
  YSockFE.start sockfe

let sql_props = Property.Map.singleton Be_sql_property.Key.url "postgresql://postgres:postgres4test@localhost:5432"


let run_yaksd without_storage http_port sock_port = 
  try%lwt
    let engine = YEngine.make () in
    YEngine.add_backend engine (Yaks_be_mm.MainMemoryBEF.make empty_properties) >>= fun _ ->
    YEngine.add_backend engine (Yaks_be_sql.SQLBEF.make sql_props) >>= fun _ ->
    let def_store = add_default_storage engine without_storage >>= fun _ -> Lwt.return_unit in
    let rest_fe = add_rest_fe engine http_port in
    let sock_fe = add_socket_fe engine sock_port in
    Lwt.join [def_store; rest_fe; sock_fe]
  with 
  | YException e  -> 
    Logs_lwt.err (fun m -> m "Exception %s raised:\n%s" (show_yerror e) (Printexc.get_backtrace ())) >> Lwt.return_unit
  | exn -> 
    Logs_lwt.err (fun m -> m "Exception %s raised:\n%s" (Printexc.to_string exn) (Printexc.get_backtrace ())) >> Lwt.return_unit


let run without_storage http_port sock_port style_renderer level = 
  setup_log style_renderer level; 
  Lwt_main.run @@ run_yaksd without_storage http_port sock_port


let () =
  Printexc.record_backtrace true;
  let env = Arg.env_var "YAKSD_VERBOSITY" in
  let _ = Term.(eval (const run $ without_storage $ http_port $ sock_port $ Fmt_cli.style_renderer () $ Logs_cli.level ~env (), Term.info "Yaks daemon")) in  ()
