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

(* 
  
  Property.Backend.Value.memory
 *)
let get_property prop ps = 
  List.find_opt (fun p -> Property.key p = prop) ps 

let has_property prop ps = match get_property prop ps with | Some _ -> true | None -> false 
 
 