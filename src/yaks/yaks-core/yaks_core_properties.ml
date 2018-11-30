module Property = struct

  include Yaks_properties

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
  module Backend = struct
    module Key = struct 
      let key = "is.yaks.backend"    
      let kind = "is.yaks.backend.kind"    
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
