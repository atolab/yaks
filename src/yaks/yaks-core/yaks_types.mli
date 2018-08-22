module EventStream  : sig 
  include (module type of Apero.EventStream)
end 

module SubscriberId : Apero.Id.S 
module PluginId : Apero.Id.S

module KeyValue : sig 
  include (module type of Apero.KeyValueF.Make (String) (String))
end [@@deriving show]

module Path : sig
  type t
  val is_key : t -> bool
  val key : t -> string option
  val prefix : t -> t
  val of_string : string -> t option
  val to_string : t -> string
  val is_prefix : t -> t -> bool
  val matches : t -> t -> bool
end [@@deriving show]

module Selector : sig
  type t          
  val of_string : string -> t option
  val to_string : t -> string
  val path : t -> Path.t
  val key : t -> string option
  val query : t -> string option
  val fragment : t -> string option
  val match_path : t -> Path.t -> bool
  val match_string : t -> string -> bool
  val of_string_list : string list -> t list
  val to_string_list : t list -> string list
end [@@deriving show]

type error_info = [`NoMsg | `Msg of string | `Code of int | `Pos of (string * int * int * int) | `Loc of string] [@@deriving show]  

type yerror = [
  | `InsufficientStorage
  | `Forbidden of error_info
  | `InvalidParameters
  | `UnknownStorage of error_info
  | `UnknownAccess of error_info
  | `UnknownStorageKind 
  | `UnavailableStorageFactory of error_info
  | `UnkownAccessId of error_info
  | `StoreError of error_info
  | `UnauthorizedAccess of error_info
  | `UnsupportedTranscoding of error_info
  | `UnsupportedOperation
] [@@deriving show]


exception YException of yerror [@@deriving show]

module Value : sig 
  type encoding = 
    | Raw_Encoding
    | String_Encoding 
    | Json_Encoding  

  type t  = 
    | RawValue of Lwt_bytes.t 
    | StringValue of string
    | JSonValue of string


  val make : Lwt_bytes.t -> encoding ->  t  
  val update : t -> t -> (t, yerror) Apero.Result.t
  val encoding : t -> encoding
  val transcode : t -> encoding -> (t, yerror) Apero.Result.t   
  val of_string : string -> encoding -> (t, yerror) Apero.Result.t 
  val to_string : t -> string  
  val to_bytes : t -> Lwt_bytes.t
  val of_bytes : Lwt_bytes.t -> encoding -> t 
end
