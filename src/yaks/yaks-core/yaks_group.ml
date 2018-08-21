open Yaks_types

module Group = struct

  module Id = struct
    include Apero.Uuid
  end [@@deriving show]



  type group_level =  Admins | Users

  type t = 
    { id : Id.t 
    ; name : string
    ; rw_paths : Selector.t list
    ; r_paths : Selector.t list
    ; w_paths : Selector.t list
    ; group_level : group_level
    } 



  let make name rw_paths r_paths w_paths group_level = { id = Id.make (); name; rw_paths; r_paths; w_paths; group_level }

  (* let id g = g.id
     let name g = g.name  *)
  (* let rw_paths g = g.rw_paths
     let r_paths g = g.r_paths
     let w_paths g = g.w_paths *)

end  [@@deriving show]

