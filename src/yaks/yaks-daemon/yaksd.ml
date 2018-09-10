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
  let open Apero.LwtM.InfixM in 
  (* let module M = Yaks_sec_dum.DummySecurity.Make(Apero.MVar_lwt) in  *)
  let module YEngine = Yaks_core.SEngine.Make (Apero.MVar_lwt) in
  let module YRestFE = Yaks_fe_rest.Make (YEngine) in 
  let module YSockFE = Yaks_fe_sock.Make (YEngine) in 
  let engine = YEngine.make () in

  try%lwt 
    YEngine.add_backend_factory engine (Property.Backend.Value.memory) (module Yaks_be_mm.MainMemoryBEF : BackendFactory) >>= 
    fun _ -> 
    let props = Property.Map.add Property.Backend.Key.key Property.Backend.Value.memory Property.Map.empty
    in YEngine.create_storage engine (Apero.Option.get @@ Path.of_string "/") props  >>=
    fun _ ->
    let restfe_cfg = YRestFE.{ port = 8000 } in
    let restfe = YRestFE.create restfe_cfg  engine in 
    let rfep = YRestFE.start restfe in 
    let socket_cfg = YSockFE.Config.create (Apero.Option.get @@ Apero_net.TcpLocator.of_string "tcp/0.0.0.0:7887") in 
    let sockfe = YSockFE.create socket_cfg engine in 
    let sfep = YSockFE.start sockfe
    in Lwt.join [rfep; sfep]

  with 
  | YException e  -> Logs_lwt.err (fun m -> m "%s" (show_yerror e)) >> Lwt.return_unit
  | exn -> Logs_lwt.err (fun m -> m "Exception %s raised" (Printexc.to_string exn)) >> Lwt.return_unit

let () =  
  Printexc.record_backtrace true;  
  let _ = Term.(eval (setup_log, Term.info "tool")) in  
  Lwt_main.run @@ yaksd ()
