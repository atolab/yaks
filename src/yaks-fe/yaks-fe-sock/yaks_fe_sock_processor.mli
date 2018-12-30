open Yaks_core
open Yaks_fe_sock_types
open Yaks_fe_sock_codes

module Processor : sig

  module type S = sig 

    module YEngine : Yaks_engine.Engine.S 
    
    val process_login : YEngine.t -> ClientId.t -> message -> message Lwt.t
    val process_logout : YEngine.t -> ClientId.t -> message -> message Lwt.t
    val process_workspace : YEngine.t -> ClientId.t -> message -> message Lwt.t
    val process_put : YEngine.t -> ClientId.t -> message -> message Lwt.t
    val process_get : YEngine.t -> ClientId.t -> message -> message Lwt.t
    val process_delete : YEngine.t -> ClientId.t -> message -> message Lwt.t
    val process_sub : YEngine.t -> ClientId.t -> message -> notify_subscriber -> message Lwt.t    
    val process_unsub : YEngine.t -> ClientId.t -> message -> message Lwt.t
    val process_eval : YEngine.t -> ClientId.t -> message -> get_on_eval -> message Lwt.t
    val process_values : message -> Value.t Lwt.u -> message Lwt.t
    val process_error :  message -> error_code -> message Lwt.t
  end

  module Make (Engine : Yaks_engine.Engine.S) : S with type YEngine.t = Engine.t

end 