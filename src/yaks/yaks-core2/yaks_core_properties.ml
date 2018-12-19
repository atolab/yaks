module Property = struct

  include Yaks_properties

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
