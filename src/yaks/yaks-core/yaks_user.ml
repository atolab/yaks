
module User = struct
  
  module Id = struct
    include Apero.Uuid
  end [@@deriving show]
  
  type t = 
    { id : Id.t 
    ; name : string
    ; password : string
    } 

  let make name password = { id = Id.next_id (); name; password }
  
  let id a = a.id
  let name a = a.name 
  let pwd a = a.password

end  [@@deriving show]
      
   