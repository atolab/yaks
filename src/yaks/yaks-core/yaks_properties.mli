module Property : sig 
  module Auth : sig 
    module Key : sig
      val key : string
      val login : string 
      val code : string
    end
  end 
  module User : sig 
    module Key : sig
      val key : string
      val id : string 
      val token : string 
    end
  end
  module Group : sig   
    module Key : sig 
      val key : string
      val id : string 
      val rws : string 
      val rs : string 
      val ws : string   
    end
  end
  module Access : sig           
    module Key : sig
      val key : string    
      val id : string 
      val alias : string
      val cache_size : string 
      val subscription_id : string
    end
  end
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
