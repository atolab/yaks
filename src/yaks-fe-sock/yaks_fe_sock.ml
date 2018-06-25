open Yaks_core
open Yaks_codec.Message

type config = { iface : string; port : int; backlog : int; bufsize : int; stream_len : int }

type t = { socket : Lwt_unix.file_descr;  engine: Engine.t; cfg : config }

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
  let (esrc, esink) = EventStream.create fe.cfg.stream_len in 
  let max_len = fe.cfg.bufsize in
  let rbuf = Lwt_bytes.create fe.cfg.bufsize in 
  let sbuf = Bi_outbuf.create fe.cfg.bufsize in 
  let ic = Lwt_io.of_fd Lwt_io.input sock in   
  let oc = Lwt_io.of_fd Lwt_io.output sock in   
  
  let rec receive_loop () = 
    let%lwt len = Lwt_io.read_int ic in  
    let%lwt bs = Lwt_bytes.read sock rbuf 0 max_len in    
    let msg = read_message @@ Bi_inbuf.from_bytes (Lwt_bytes.to_bytes rbuf) in    
    let%lwt _ = Engine.process fe.engine msg esink in 
    receive_loop ()    
  in 
  
  let rec send_loop () =     
    match%lwt EventStream.Source.get esrc with 
    | Some msg ->    
      write_message sbuf msg ;
      let payload = Bi_outbuf.contents sbuf in 
      let len = String.length payload in
      let%lwt _ = Lwt_io.write_int oc len in 
      let%lwt _ = Lwt_unix.send sock (Bytes.of_string payload) 0 len [] in
      Lwt.return_unit
    | None -> send_loop ()
  
  in

  Lwt.pick [receive_loop (); send_loop ()]

let rec accept_connection fe = 
  let%lwt _ = Logs_lwt.debug (fun m -> m "Socket-FE ready to accept connection" ) in
  let%lwt (sock, addr) = Lwt_unix.accept fe.socket in
  let _ = serve_connection sock fe in
  accept_connection fe

let create cfg engine = 
  let _ = Logs_lwt.debug (fun m -> m "Socket-FE creating accepting socket") in
  let socket = create_socket cfg  in  
  { socket; engine; cfg }

let start = accept_connection 

let stop fe = Lwt.return_unit
  (** TODO: Implement this.  *)
   


   