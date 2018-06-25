module EventStream = struct 
  
  module type S = sig 
    type 'a t          
    
    module Sink : sig
      type 'a s            
      val of_stream : 'a t -> 'a s
      val push : 'a -> 'a s -> unit Lwt.t    
    end

    module Source : sig             
      type 'a s 
      val of_stream : 'a t -> 'a s
      val get : 'a s -> 'a option Lwt.t
    end    

    val create : int -> 'a Source.s * 'a Sink.s 
  end  
  

   module Make (I : sig 
                    type 'a q 
                    val create : int -> 'a q
                    val push : 'a -> 'a q -> unit Lwt.t  
                    val get : 'a q -> 'a option Lwt.t
                  end ) : S with type 'a t = 'a I.q = struct 

      type 'a t = 'a I.q
             
    module Sink = struct
      type 'a s  = 'a I.q
      
      let of_stream s = s
      let push = I.push 
    end

    module  Source = struct
      type 'a s = 'a I.q 
      
      let of_stream s = s
      let get = I.get 
    end  

    let create len = 
      let s = I.create len in 
      (Source.of_stream s, Sink.of_stream s)           
    end
end
