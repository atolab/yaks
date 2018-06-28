open Yaks_core
open Yaks_codec.Message

type config = { iface : string; port : int; backlog : int; bufsize : int; stream_len : int }

type t = { socket : Lwt_unix.file_descr;  sink: (message * message EventStream.Sink.s) EventStream.Sink.s; cfg : config }

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
  let%lwt _ = Logs_lwt.debug (fun m -> m "Serving connection" ) in
  let (esrc, esink) = EventStream.create fe.cfg.stream_len in 
  let max_len = fe.cfg.bufsize in
  let rbuf = Lwt_bytes.create fe.cfg.bufsize in 
  let sbuf = Bi_outbuf.create fe.cfg.bufsize in    
  
  let rec receive_loop () = 
    let%lwt _ = Logs_lwt.debug (fun m -> m "Watiting for connection data" ) in
    let lbuf = Lwt_bytes.create 4 in 
    let%lwt n = Lwt_bytes.recv sock lbuf 0 4 [] in 
    if n != 0 then 
      begin  
        let%lwt len = Lwt_io.read_int (Lwt_io.of_bytes Lwt_io.Input lbuf)in  
        let%lwt _ = Logs_lwt.debug (fun m -> m "Received message of %d bytes" len) in
        let%lwt n = Lwt_bytes.recv sock rbuf 0 max_len [] in    
        if n != 0 then 
          begin
            let%lwt _ = Logs_lwt.debug (fun m -> m "Read %d bytes out of the socket" n) in
            try 
              let msg = read_message @@ Bi_inbuf.from_bytes (Lwt_bytes.to_bytes rbuf) in    
              let%lwt _ = EventStream.Sink.push (msg, esink) fe.sink in  receive_loop ()          
            with 
            | _ -> let%lwt _ = Logs_lwt.debug (fun m -> m "Failed to decode the message!") in receive_loop ()
          end
        else 
          let%lwt _ = Logs_lwt.debug (fun m -> m "Peer closed session") in Lwt.return_unit
      end
    else 
      let%lwt _ = Logs_lwt.debug (fun m -> m "Peer closed session") in Lwt.return_unit
  in 
  
  let rec send_loop () =     
    match%lwt EventStream.Source.get esrc with 
    | Some msg ->    
      write_message sbuf msg ;
      let payload = Bi_outbuf.contents sbuf in 
      let len = String.length payload in
      let lbuf = Lwt_bytes.create 4 in       
      let oc = (Lwt_io.of_bytes Lwt_io.Output lbuf) in
      let%lwt _ = Lwt_io.write_int oc len in 
      let%lwt _ = Lwt_bytes.send sock lbuf 0 (Lwt_bytes.length lbuf) [] in                         
      let%lwt _ = Lwt_bytes.send sock (Lwt_bytes.of_string payload) 0 len [] in                         
      Lwt.return_unit
    | None -> send_loop ()
  
  in

  Lwt.pick [receive_loop (); send_loop ()]

let rec accept_connection fe = 
  let%lwt _ = Logs_lwt.debug (fun m -> m "Socket-FE ready to accept connection" ) in
  let%lwt (sock, addr) = Lwt_unix.accept fe.socket in
  let _ = serve_connection sock fe in
  accept_connection fe

let create cfg sink = 
  let _ = Logs_lwt.debug (fun m -> m "Socket-FE creating accepting socket") in
  let socket = create_socket cfg  in  
  { socket; sink; cfg }

let start = accept_connection 

let stop fe = Lwt.return_unit
  (** TODO: Implement this.  *)
   


   