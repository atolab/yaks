open Apero
open Yaks_types
open Yaks_core_types
open Yaks_be
open Yaks_storage

module AdminSpace : sig

  module type S = sig 
    type t
    val make : Yid.t -> Zenoh.t option -> t

    val login : t -> ClientId.t -> properties -> unit Lwt.t
    val logout : t -> ClientId.t -> unit Lwt.t

    val add_workspace : t -> ClientId.t -> Path.t -> WsId.t Lwt.t

    val get : t -> ClientId.t -> Selector.t -> (Path.t * Value.t) list  Lwt.t
    val put : t -> ClientId.t -> Path.t -> Value.t -> unit Lwt.t
    val remove : t -> ClientId.t -> Path.t -> unit Lwt.t

    val create_subscriber : t -> ClientId.t -> Selector.t -> bool -> notify_subscriber -> SubscriberId.t Lwt.t  
    val remove_subscriber : t -> ClientId.t -> SubscriberId.t -> unit Lwt.t
    val notify_subscribers : t -> Path.t -> Value.t -> unit Lwt.t

    val create_eval : t -> ClientId.t -> Path.t -> get_on_eval -> unit Lwt.t
    val remove_eval : t -> ClientId.t -> Path.t -> unit Lwt.t
    val get_on_evals : t -> ClientId.t -> int -> Selector.t -> (Path.t * Value.t list) list  Lwt.t

    val get_workspace_path : t -> ClientId.t -> WsId.t -> Path.t Lwt.t

    val get_storages_for_path : t -> Path.t -> Storage.t list Lwt.t
    val get_storages_for_selector : t -> Selector.t -> Storage.t list Lwt.t

    

    (* TODO: Temporary operations that should be replaced by put/get/remove usage *)
    val add_backend_TMP : t -> (module Backend) -> unit Lwt.t
    val add_frontend_TMP : t -> string -> properties -> unit Lwt.t

  end

  module Make (MVar: Apero.MVar) : S 

end
