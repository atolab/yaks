module Stream = struct   
  
  type 'a q = {stream : 'a Lwt_stream.t; pusher: 'a -> unit Lwt.t }
  
  let create len = 
    let (stream, p) = Lwt_stream.create_bounded len in 
    { stream; pusher = fun e -> p#push e } 

  let push e s = s.pusher e 

  let get s = Lwt_stream.get s.stream  

end

