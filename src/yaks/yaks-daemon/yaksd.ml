open Lwt
open Lwt.Infix
open Yaks_core
open Cmdliner

let listen_address = Unix.inet_addr_any
let port = 8448
let backlog = 10
let max_buf_len = 64 * 1024


let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup_log =
  let env = Arg.env_var "YAKSD_VERBOSITY" in
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ~env ())


let yaksd () = 
  let ecfg = Engine.{channel_len = 32 } in
  let engine = Engine.create  ecfg in 
  let sockfecfg = Yaks_fe_sock.{ iface = "127.0.0.1"; port = 8448; backlog = 10; bufsize = 64000; stream_len = 32 } in   
  let sockfe = Yaks_fe_sock.create sockfecfg (Engine.event_sink engine) in
  let restfecfg = Yaks_fe_rest.{ port = 8000; stream_len = 32 } in   
  let restfe = Yaks_fe_rest.create restfecfg (Engine.event_sink engine) in
  Lwt.join [Engine.start engine; Yaks_fe_sock.start sockfe; Yaks_fe_rest.start restfe]


let () =  
  let _ = Term.(eval (setup_log, Term.info "tool")) in  
  Lwt_main.run @@ yaksd ()
