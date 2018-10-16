open Yaks_sock_types


module Message : sig 

  type t
  type entity_type = [
    | `Access
    | `Storage
    | `Resource
  ]

  type id =
    | IdAccess of AccessId.t
    | IdStorage of StorageId.t
    | IdSubscription of SubscriberId.t

  val make_msg : ?corrid:int64 -> Yaks_fe_sock_codes.message_id -> Yaks_fe_sock_codes.message_flags list -> String.t Yaks_core.Property.Map.t -> Yaks_fe_sock_types.payload -> Yaks_fe_sock_types.message Lwt.t
  (* val get_properties : t -> (String.t Yaks_core.Property.Map.t) Lwt.t
     val get_body : t -> Yaks_fe_sock_types.payload Lwt.t
     val get_encoding : t -> Yaks_fe_sock_codes.value_encoding Lwt.t *)

  val make_open : ?username:String.t -> ?password:String.t -> unit -> Yaks_fe_sock_types.message Lwt.t
  val make_create : ?alias:String.t -> ?config:String.t -> ?complete:bool -> entity_type -> Yaks_core.Path.t -> int -> Yaks_fe_sock_types.message Lwt.t
  val make_delete : ?delete_type:entity_type -> ?path:Yaks_core.Path.t -> id -> Yaks_fe_sock_types.message Lwt.t
  val make_put : ?encoding: Yaks_fe_sock_codes.value_encoding -> id ->  Yaks_core.Selector.t -> Yaks_core.Value.t -> Yaks_fe_sock_types.message Lwt.t 
  val make_patch : ?encoding: Yaks_fe_sock_codes.value_encoding -> id -> Yaks_core.Selector.t -> Yaks_core.Value.t -> Yaks_fe_sock_types.message Lwt.t 
  val make_get : ?encoding: Yaks_fe_sock_codes.value_encoding -> id -> Yaks_core.Selector.t -> Yaks_fe_sock_types.message Lwt.t
  val make_sub : ?encoding: Yaks_fe_sock_codes.value_encoding -> id -> Yaks_core.Selector.t -> Yaks_fe_sock_types.message Lwt.t
  val make_unsub : id ->  id -> Yaks_fe_sock_types.message Lwt.t 
  val make_values : ?encoding: Yaks_fe_sock_codes.value_encoding -> id -> (Yaks_core.Path.t * Yaks_core.Value.t) list -> Yaks_fe_sock_types.message Lwt.t
  val make_ok : id -> int64 -> Yaks_fe_sock_types.message Lwt.t
  val make_error : id -> int64 -> Yaks_fe_sock_codes.error_code -> Yaks_fe_sock_types.message Lwt.t
end
