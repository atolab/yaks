open Yaks_core
open Yaks_codec.Message

type config = { iface : string; port : int; backlog : int; bufsize : int; stream_len : int }

type t = { socket : Lwt_unix.file_descr; estream : message EventStream.t;  engine: Engine.t; cfg : config }

(** TODO: This should also be a functor configured by the serialier  *)

let create_socket cfg = 
  let open Lwt_unix in      
    let sock = socket PF_INET SOCK_STREAM 0 in
    let _ = setsockopt sock SO_REUSEADDR true in
    let _ = setsockopt sock TCP_NODELAY true in
    let addr = Unix.inet_addr_of_string cfg.iface in
    let saddr = ADDR_INET (addr, cfg.port) in    
    let _ = bind sock saddr in
    let _ = listen sock cfg.backlog in sock

let serve_connection sock fe = 
  let es = EventStream.create fe.cfg.stream_len in 
  let max_len = fe.cfg.bufsize in
  let buf = Lwt_bytes.create fe.cfg.bufsize in 
  let ic = Lwt_io.of_fd Lwt_io.input sock in     
  
  let rec serve_loop () = 
    let%lwt len = Lwt_io.read_int ic in  
    let%lwt bs = Lwt_bytes.read sock buf 0 max_len in    
    let msg = read_message @@ Bi_inbuf.from_bytes (Lwt_bytes.to_bytes buf) in    
    let%lwt _ = Engine.process fe.engine msg (EventStream.Sink.of_stream es) in 
    serve_loop ()
    
  in serve_loop ()


let rec accept_connection fe = 
  let%lwt _ = Logs_lwt.debug (fun m -> m "Socket-FE ready to accept connection" ) in
  let%lwt (sock, addr) = Lwt_unix.accept fe.socket in
  let _ = serve_connection sock fe in
  accept_connection fe

let create cfg engine = 
  let socket = create_socket cfg  in
  let estream = EventStream.create cfg.stream_len in
  { socket; estream; engine; cfg }

let start = accept_connection 

let stop fe = Lwt.return_unit
  (** TODO: Implement this.  *)
   


   