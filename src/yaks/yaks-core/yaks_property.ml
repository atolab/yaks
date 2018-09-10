module Property = struct 
  include Apero.KeyValueF.Make (String) (String)   
  module Auth = struct
    module Key = struct 
      let key = "is.yaks.auth"
      let login = "is.yaks.auth.login"
      let code = "is.yaks.auth.code"    
    end
  end
  module User = struct 
    module Key = struct 
      let key = "is.yaks.user"
      let id = "is.yaks.user.id"     
      let token = "is.yaks.user.token"     
    end
  end 
  module Group = struct 
    module Key = struct 
      let key = "is.yaks.group"
      let id = "is.yaks.group.id" 
      let rws = "is.yaks.group.rws" 
      let rs = "is.yaks.group.rs" 
      let ws = "is.yaks.group.ws" 
    end
  end 
  module Access = struct               
    module Key = struct 
      let key = "is.yaks.access" 
      let id = "is.yaks.access.id" 
      let alias = "is.yaks.access.alias" 
      let cache_size = "is.yaks.access.cachesize" 
    end
  end
  module Storage = struct
    module Key = struct 
      let key = "is.yaks.storage"
      let id = "is.yaks.storage.id"    
      let config = "is.yaks.storage.config" 
      let set = "is.yaks.storage.set"
    end
  end  
  module Backend = struct
    module Key = struct 
      let key = "is.yaks.backend"    
    end
    module Value = struct
      let memory = "memory"
      let dbms = "dbms"
    end
  end
  module Frontend = struct
    module Key = struct 
      let key = "is.yaks.frontend"
    end
    module Value = struct
      let rest = "REST"
      let socket = "socket"
    end
  end
end [@@deriving show]

type properties = Property.Value.t Property.Map.t


let rec properties_of_list = function
  | [] -> Property.Map.empty
  | (k,v)::l -> Property.Map.add k v @@ properties_of_list l

let list_of_properties p =
  Property.Map.fold (fun k v l -> (k,v)::l) p []


let get_property prop ps = Property.Map.find_opt prop ps

let decode_property_value decoder prop ps = 
  let open Apero.Option.Infix in 
  get_property prop ps 
  >>= fun v -> 
  try 
    Some (decoder v)
  with 
  | _ -> None

let encode_property_value encoder v = 
  try 
    Some (encoder v)
  with 
  | _ -> None

let has_property prop ps = match get_property prop ps with | Some _ -> true | None -> false 

let has_same_property k v ps = 
  match get_property k ps with
  | Some v' -> v = v'
  | None -> false  


let is_subset ps ps' = not @@ Property.Map.exists (fun k v -> not @@ has_same_property k v ps') ps
