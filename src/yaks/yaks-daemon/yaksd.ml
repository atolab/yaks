(* open Lwt *)
(* open Lwt.Infix *)
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


(* let exec () =
   let open Actor.Actor in
   let mm_actor, mm_loop = Yaks_be_mm.create "" in
   (* let _ = (mm_actor <!> (None, Create { cid=Int64.of_int 112; entity = Storage {path = "/root/home"; properties = [] }; entity_id = StorageId "112" } )) *)
   let _ = mm_loop in Lwt.return_unit  *)

(* let yaksd () = 
  let mm_actor, mm_loop = Yaks_be_mm.create "" in
  let ecfg = Engine.{channel_len = 32 } in
  (* let engine = Engine.create ecfg mm_actor in  *)
  let engine = Engine.create ecfg in

  let msg = Event (AddPlugin{entity=Plugin{mailbox=mm_actor; kind=Backend};entity_id=PluginId(0L)}) in
  let _ = Actor.Actor.send (Engine.mailbox engine) None msg in
  (* let _, e_state = Engine.add_febe mm_actor (FEBEId (1L)) Backend e_state in *)
  let sockfecfg = Yaks_fe_sock.{ iface = "127.0.0.1"; port = 8448; backlog = 10; bufsize = 64000; stream_len = 32 } in   
  let sockfe = Yaks_fe_sock.create sockfecfg (Engine.mailbox engine) in
  let restfecfg = Yaks_fe_rest.{ port = 8000 } in
  let restfe = Yaks_fe_rest.create restfecfg (Engine.mailbox engine) in
  Lwt.join [Engine.start engine; Yaks_fe_sock.start sockfe; Yaks_fe_rest.start restfe; mm_loop ] *)


let yaksd_mvar () = 
  let open Apero.LwtM.InfixM in 
  let module M = Yaks_sec_dum.DummySecurity.Make(Apero.MVar_lwt) in 
  let module YEngine = Yaks_core.SEngine.Make (Apero.MVar_lwt)(M) in
  let module YRest = Yaks_fe_rest_mvar.Make (YEngine) in 
  let engine = YEngine.make () in
  
  try%lwt 
    YEngine.add_backend_factory engine (yaks_backend_memory) (module Yaks_bef_mm.MainMemoryBEF : BackendFactory) >>= 
    fun _ -> YEngine.create_storage engine (Apero.Option.get @@ Path.of_string "/") [Property.make yaks_backend yaks_backend_memory] >>=
    fun _ ->
      let restfecfg = YRest.{ port = 8000 } in
      let restfe = YRest.create restfecfg  engine in YRest.start restfe
  with 
  | YException e  -> Logs_lwt.err (fun m -> m "%s" (show_yerror e)) >> Lwt.return_unit
  | exn -> Logs_lwt.err (fun m -> m "Exception %s raised" (Printexc.to_string exn)) >> Lwt.return_unit

  

let () =  
  Printexc.record_backtrace true;  
  let _ = Term.(eval (setup_log, Term.info "tool")) in  
  Lwt_main.run @@ yaksd_mvar ()
