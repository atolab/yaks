open Yaks_types

module Access = struct
  
  module Id = struct
    include Apero.Uuid
  end [@@deriving show]
  
  type access_right =  R_Mode | W_Mode | RW_Mode

  type t = 
    { id : Id.t 
    ; path : Path.t 
    ; cache_size : int64 } 

  let make path cache_size = { id = Id.next_id (); path; cache_size }
  
  let id a = a.id
  let path a = a.path 
  let cache_size a = a.cache_size

  let check_access_right _ _ _ = Lwt.return_unit 

end  [@@deriving show]
      
   