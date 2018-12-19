module Property : sig 
  module Storage : sig 
    module Key : sig
      val key : string
      val id : string
      val alias : string
      val config : string 
      val set : string
    end
  end
  module Backend : sig
    module Key : sig 
      val key : string 
      val kind : string 
    end
    module Value : sig
      val memory : string
      val dbms : string
    end
  end
  module Frontend : sig
    module Key : sig
      val key : string
    end
    module Value : sig
      val rest : string
      val socket : string
    end
  end
end [@@deriving show]
