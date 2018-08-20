module Property : sig 
  include (module type of Apero.KeyValueF.Make (String) (String))  
end [@@deriving show]


val yaks_backend : string 
val yaks_backend_memory : string
val yaks_auth : string

val get_property : string -> Property.t list -> Property.t option
val has_property : string -> Property.t list -> bool

(*
  let yaks_backend_dbms = "dbms" (* any DBMS *)
  let yaks_backend_dbms = "postgresql"
  let yaks_backend_dbms = "mariadb"
  let yaks_backend_dbms = "sqlite"
*)

