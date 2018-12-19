open Apero
open Yaks_types
open Yaks_core_types


module SEngine : sig 

  module type S = sig 
    type t 

    val make : unit -> t 

    val get : t -> ClientId.t -> ?workspace:WsId.t -> Selector.t -> (Path.t * Value.t) list  Lwt.t
    val put : t -> ClientId.t -> ?workspace:WsId.t -> Path.t -> Value.t -> unit Lwt.t
    val put_delta : t -> ClientId.t -> ?workspace:WsId.t -> Path.t -> Value.t -> unit Lwt.t
    val remove : t -> ClientId.t -> ?workspace:WsId.t -> Path.t -> unit Lwt.t

    val create_subscriber : t -> ClientId.t -> ?workspace:WsId.t -> Selector.t -> bool -> notify_subscriber -> SubscriberId.t Lwt.t  
    val remove_subscriber : t -> ClientId.t -> ?workspace:WsId.t -> SubscriberId.t -> unit Lwt.t  

    val eval : t -> ClientId.t -> ?workspace:WsId.t -> Path.t -> get_on_eval -> unit Lwt.t
  end

  module Make (MVar: Apero.MVar) : S 

end
