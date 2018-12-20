open Apero
open Yaks_types
open Yaks_core_types
open Yaks_be


module Engine : sig 

  module type S = sig 
    type t 

    val make : unit -> t 

    val get : t -> ClientId.t -> ?quorum:int -> ?workspace:WsId.t -> Selector.t -> (Path.t * Value.t) list  Lwt.t
    val put : t -> ClientId.t -> ?quorum:int -> ?workspace:WsId.t -> Path.t -> Value.t -> unit Lwt.t
    val update : t -> ClientId.t -> ?quorum:int -> ?workspace:WsId.t -> Path.t -> Value.t -> unit Lwt.t
    val remove : t -> ClientId.t -> ?quorum:int -> ?workspace:WsId.t -> Path.t -> unit Lwt.t

    val subscribe : t -> ClientId.t -> ?workspace:WsId.t -> Selector.t -> bool -> notify_subscriber -> SubscriberId.t Lwt.t  
    val unsubscribe : t -> ClientId.t -> SubscriberId.t -> unit Lwt.t  

    val eval : t -> ClientId.t -> ?workspace:WsId.t -> Path.t -> get_on_eval -> unit Lwt.t

    (* TODO: Temporary operations that should be replaced by put/get/remove usage *)
    val add_backend_TMP : t -> string -> (module Backend) -> unit Lwt.t
    val add_frontend_TMP : t -> string -> properties -> unit Lwt.t
  end

  module Make (MVar: Apero.MVar) : S 

end
