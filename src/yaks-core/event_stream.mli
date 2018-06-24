module EventStream : sig 
  
  module type S = sig 
    type 'a t      
    val create : int -> 'a t 
    
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
  end 
  
  
  module Make (I : sig 
                    type 'a q 
                    val create : int -> 'a q
                    val push : 'a -> 'a q -> unit Lwt.t  
                    val get : 'a q -> 'a option Lwt.t
                  end ) : S with type 'a t = 'a I.q

end


