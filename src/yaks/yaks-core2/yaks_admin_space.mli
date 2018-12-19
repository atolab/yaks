open Apero
open Yaks_types
open Yaks_core_types
open Yaks_be
open Yaks_storage

module YAdminSpace : sig

  module type S = sig 
    type t
    val make : unit -> t

    val login : t -> ClientId.t -> properties -> unit Lwt.t
    val logout : t -> ClientId.t -> unit Lwt.t

    val workspace : t -> ClientId.t -> Path.t -> WsId.t Lwt.t

    val get : t -> ClientId.t -> Selector.t -> (Path.t * Value.t) list  Lwt.t
    val put : t -> ClientId.t -> Path.t -> Value.t -> unit Lwt.t
    val remove : t -> ClientId.t -> Path.t -> unit Lwt.t

    val create_subscriber : t -> ClientId.t -> Selector.t -> bool -> notify_subscriber -> SubscriberId.t Lwt.t  
    val remove_subscriber : t -> ClientId.t -> SubscriberId.t -> unit Lwt.t  

    val get_storages_for_path : t -> Path.t -> Storage.t list Lwt.t
    val get_storages_for_selector : t -> Selector.t -> Storage.t list Lwt.t

    (* TODO: Temporary operations that should be replaced by put/get/remove usage *)
    val add_backend_TMP : t -> (module Backend) -> string -> unit Lwt.t
  
  end

  module Make (MVar: Apero.MVar) : S 

end