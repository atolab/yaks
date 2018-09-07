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
  let module YRest = Yaks_fe_rest.Make (YEngine) in 
  let engine = YEngine.make () in

  try%lwt 
    YEngine.add_backend_factory engine (Property.Backend.Value.memory) (module Yaks_be_mm.MainMemoryBEF : BackendFactory) >>= 
    fun _ -> YEngine.create_storage engine (Apero.Option.get @@ Path.of_string "/") [Property.make Property.Backend.Key.key Property.Backend.Value.memory] >>=
    fun _ ->
    let restfecfg = YRest.{ port = 8000 } in
    let restfe = YRest.create restfecfg  engine in YRest.start restfe
  with 
  | YException e  -> Logs_lwt.err (fun m -> m "%s" (show_yerror e)) >> Lwt.return_unit
  | exn -> Logs_lwt.err (fun m -> m "Exception %s raised" (Printexc.to_string exn)) >> Lwt.return_unit



let () =  
  Printexc.record_backtrace true;  
  let _ = Term.(eval (setup_log, Term.info "tool")) in  
  Lwt_main.run @@ yaksd ()
