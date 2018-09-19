module Property : sig 
  include (module type of Apero.KeyValueF.Make (String) (String))
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

type properties = Property.Value.t Property.Map.t

val empty_properties : properties
val singleton_properties : string -> string -> properties
val properties_of_list : (string * string) list -> properties
val list_of_properties : properties -> (string * string) list
val string_of_properties : properties -> string

val add_property : string -> string -> properties -> properties

val get_property : string -> properties -> string option
val has_property : string -> properties -> bool
val has_same_property : string -> string -> properties -> bool
(** [has_same_property k v ps] tests if [ps] contains a property with [k] as key and [v] as value *)
val is_subset : properties -> properties -> bool
(** [is_subset p p'] tests if all properties of [p] are present in [p'] with the same values *)
val has_conflicting_property : string -> string -> properties -> bool
(** [has_conflicting_property k v ps] tests if [ps] contains a property with [k] as key and a value which is not equal to [v]
    (if [ps] doesn't contain a property [k] their is no conflict) *)
val not_conflicting : properties -> properties -> bool
(** [is_subset p p'] tests if some properties are present in both [p] and [p'] they have the same value *)

val decode_property_value : (string -> 'a) -> string -> properties -> 'a option

val encode_property_value : ('a -> string) -> 'a -> string option

