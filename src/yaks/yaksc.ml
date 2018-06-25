open Lwt
open Lwt.Infix
open Yaks_core
open Yaks_fe_sock
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

let yaksd () =   Lwt.return_unit
  
 

let () =  
  Lwt_main.run @@ yaksd ()
