module Property = Apero.KeyValueF.Make (String) (String) [@@deriving show]


let yaks_backend = "yaks.backend"
let yaks_backend_memory = "memory"
let yaks_auth = "yaks.auth"

let get_property prop ps = 
  List.find_opt (fun p -> Property.key p = prop) ps 

let has_property prop ps = match get_property prop ps with | Some _ -> true | None -> false 
 