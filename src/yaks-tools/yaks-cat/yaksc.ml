open Lwt
open Lwt.Infix
open Yaks_core
open Yaks_codec.Codec.Bin
open Yaks_fe_sock
open Cmdliner

(** TODO:
  - Use a parser framework to parse the command line.
 *)

let buf_len = 8192

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()


let setup_log =
  let env = Arg.env_var "YAKSC_VERBOSITY" in
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ~env ())

let write_prompt () = 
  let%lwt _ = Lwt_io.write_char Lwt_io.stdout '>' in 
  let%lwt _ = Lwt_io.write_char Lwt_io.stdout '>' in 
  Lwt_io.write_char Lwt_io.stdout ' ' 

let process_input sock buf oc line = 
  let%lwt _ = Logs_lwt.debug (fun m -> m "Prcessing: %s" line) in 
  let msg = { mid = 0; msg = `Request (`AOp (`Get "//test/one"))} in
  write_message buf msg; 
  let payload = Lwt_bytes.of_string @@ Bi_outbuf.contents buf in
  let len = Lwt_bytes.length payload in   
  let%lwt _ = Logs_lwt.debug (fun m -> m "Writing %d bytes" len) in 
  let lbuf = Lwt_bytes.create 4 in 
  let%lwt _ = Lwt_io.write_int (Lwt_io.of_bytes Lwt_io.Output lbuf) len in
  let%lwt _ = Lwt_bytes.send sock lbuf 0 4 [] in 
  let%lwt _ = Lwt_bytes.send sock payload 0 len [] in 
  Bi_outbuf.clear buf ; 
  Lwt.return_unit
  

let console_input_loop sock =
  let wbuf = Bi_outbuf.create 1024 in  
  let oc = Lwt_io.of_fd Lwt_io.Output sock in
  let rec loop () =     
    let%lwt _ = write_prompt () in 
    let%lwt line = Lwt_io.read_line Lwt_io.stdin in  
    process_input sock wbuf oc line >>= loop    
  in loop ()


let (addr, port) = 
  if Array.length Sys.argv < 3 then
    begin
      let ipaddr = "127.0.0.1" in
      let port = 8448 in      
      (ipaddr, port)
    end
  else (Array.get Sys.argv 1, int_of_string @@ Array.get Sys.argv 2)
 
let connect addr port = 
  let open Lwt_unix in 
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true ;
  setsockopt sock TCP_NODELAY true ;
  let saddr = ADDR_INET (Unix.inet_addr_of_string addr, port) in
  let%lwt _ =  connect sock  saddr in return sock 


let net_input_loop sock =   
  let lbuf = Lwt_bytes.create 4 in
  let buf = Lwt_bytes.create buf_len in
  let rec loop () = 
    let%lwt _ = Logs_lwt.debug (fun m -> m "Waiting for networl input") in 
    
    let%lwt _ = Lwt_bytes.recv sock lbuf 0 4 [] in 
    let%lwt len = Lwt_io.read_int (Lwt_io.of_bytes Lwt_io.Input lbuf) in 
    let%lwt _ = Logs_lwt.debug (fun m -> m "Reading %d bytes off the network" len) in
    let%lwt _ = Lwt_bytes.recv sock buf 0 len [] in 
    let bif = Bi_inbuf.from_bytes (Lwt_bytes.to_bytes buf)  in 
    let msg = read_message bif in
    let%lwt _ = Logs_lwt.debug (fun m -> m "Received message") in 
    loop ()
  in loop ()


let yaksc sock =    
  Lwt.join [console_input_loop sock; net_input_loop sock ]
  
 

let () =  
  let _ = Term.(eval (setup_log, Term.info "tool")) in  
  Lwt_main.run @@ (connect addr port >>= yaksc)
