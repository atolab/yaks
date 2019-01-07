open Apero
open Yaks_types
open Yaks_core_types
open Yaks_be


module Engine : sig 

  module type S = sig 
    type t 

    val make : Zenoh.t option -> t

    val id : Yid.t

    val login : t -> ClientId.t -> properties -> unit Lwt.t
    val logout : t -> ClientId.t -> unit Lwt.t   

    val add_workspace : t -> ClientId.t -> Path.t -> WsId.t Lwt.t

    val get : t -> ClientId.t -> ?quorum:int -> ?workspace:WsId.t -> Selector.t -> (Path.t * Value.t) list  Lwt.t
    val put : t -> ClientId.t -> ?quorum:int -> ?workspace:WsId.t -> Path.t -> Value.t -> unit Lwt.t
    val update : t -> ClientId.t -> ?quorum:int -> ?workspace:WsId.t -> Path.t -> Value.t -> unit Lwt.t
    val remove : t -> ClientId.t -> ?quorum:int -> ?workspace:WsId.t -> Path.t -> unit Lwt.t

    val subscribe : t -> ClientId.t -> ?workspace:WsId.t -> Selector.t -> bool -> notify_subscriber -> SubscriberId.t Lwt.t  
    val unsubscribe : t -> ClientId.t -> SubscriberId.t -> unit Lwt.t  

    val register_eval : t -> ClientId.t -> ?workspace:WsId.t -> Path.t -> eval_function -> unit Lwt.t
    val unregister_eval : t -> ClientId.t -> ?workspace:WsId.t -> Path.t -> unit Lwt.t
    val eval : t -> ClientId.t -> ?multiplicity:int -> ?workspace:WsId.t -> Selector.t -> (Path.t * Value.t) list  Lwt.t

    (* TODO: Temporary operations that should be replaced by put/get/remove usage *)
    val add_backend_TMP : t -> (module Backend) -> unit Lwt.t
    val add_frontend_TMP : t -> string -> properties -> unit Lwt.t
  end

  module Make (MVar: Apero.MVar) : S 

end
