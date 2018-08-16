open Yaks_types

module Access = struct
  
  module Id = struct
    include Apero.Uuid
  end [@@deriving show]
  
  type access_right =  R_Mode | W_Mode | RW_Mode

  type t = 
    { id : Id.t 
    ; path : Path.t 
    ; cache_size : int64
    ; right : access_right } 

  let make path cache_size right = { id = Id.next_id (); path; cache_size; right = right }

  let make_with_id id path cache_size right = { id = id; path; cache_size; right = right }

  let id a = a.id
  let path a = a.path 
  let cache_size a = a.cache_size
  let right a = a.right

  let check_access_right _ _ _ = Lwt.return_unit 

end  [@@deriving show]
      
   