open Yaks_group


module User = struct
  
  module Id = struct
    include Apero.Uuid
  end [@@deriving show]
  
  type t = 
    { id : Id.t 
    ; name : string
    ; password : string
    ; group : Group.Id.t
    } 

  let make name password group = { id = Id.next_id (); name; password; group }
  
  (* let id u = u.id
  let name u = u.name 
  let pwd u = u.password
  let groups u = u.groups *)

  (* let add_group u gid = {u with groups = u.groups @ [gid]}
  let remove_group u gid = {u with groups = List.filter (fun e -> e = gid) u.groups} *)


end  [@@deriving show]
      
   