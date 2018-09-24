open Yaks_fe_sock_types
open Yaks_fe_sock_codes
open Apero_net 
module Processor : sig

  module type S = sig 

    module YEngine : Yaks_engine.SEngine.S 
    
    val process_open : YEngine.t -> message -> message Lwt.t
    val process_create_access : YEngine.t -> message -> Yaks_types.Path.t -> message Lwt.t
    val process_create_storage : YEngine.t -> message -> Yaks_types.Path.t -> message Lwt.t
    val process_create : YEngine.t -> message -> message Lwt.t
    val process_delete_access : YEngine.t -> message -> message Lwt.t
    val process_delete_storage : YEngine.t -> message -> message Lwt.t
    val process_delete_tuple : YEngine.t -> message -> Yaks_types.Selector.t -> message Lwt.t
    val process_delete : YEngine.t -> message -> message Lwt.t
    val process_put : YEngine.t -> message -> message Lwt.t
    val process_get : YEngine.t -> message -> message Lwt.t
    val process_sub : YEngine.t -> TxSession.t -> message ->  message Lwt.t
    val process_unsub : YEngine.t -> message -> message Lwt.t
    val process_eval : YEngine.t -> message -> message Lwt.t
    val process_error :  message -> error_code -> message Lwt.t
  end

  module Make (Engine : Yaks_engine.SEngine.S) : S with type YEngine.t = Engine.t

end 