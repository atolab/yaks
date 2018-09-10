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
      let alias = "is.yaks.storage.alias"    
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

(* 
  
  Property.Backend.Value.memory
 *)
let get_property key ps = 
  List.find_opt (fun p -> Property.key p = key) ps 

let get_property_value key ps = 
  let open Apero.Option.Infix in 
  get_property key ps >|= fun (_,v) -> v

let decode_property_value decoder prop ps = 
  let open Apero.Option.Infix in 
  get_property prop ps 
  >>= fun (_, v) -> 
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
 
 