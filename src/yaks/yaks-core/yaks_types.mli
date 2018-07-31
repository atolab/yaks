module Property : sig 
  include (module type of Apero.KeyValueF.Make (String) (String))
end [@@deriving show]

module EventStream  : sig 
  include (module type of Apero.EventStream)
end 

module AccessId : sig 
  type t
  val make : unit -> t
  val next_id : unit -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val of_bytes : ?pos:int -> string -> t option
  val to_bytes : t -> string
  val of_string : ?pos:int -> string -> t option
  val to_string : ?upper:bool -> t -> string
end [@@deriving show]

module StorageId : sig 
  type t
  val make : unit -> t
  val next_id : unit -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val of_bytes : ?pos:int -> string -> t option
  val to_bytes : t -> string
  val of_string : ?pos:int -> string -> t option
  val to_string : ?upper:bool -> t -> string
end [@@deriving show]

module SubscriberId : Apero.Id.S 
module PluginId : Apero.Id.S

module KeyValue : sig 
 include (module type of Apero.KeyValueF.Make (String) (String))
end [@@deriving show]

module Path : sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val is_prefix : t -> t -> bool
  val matches : t -> t -> bool
end [@@deriving show]

module Selector : sig

  type t          
  val of_string : string -> t
  val to_string : t -> string
  val path : t -> Path.t
  val query : t -> string option
  val fragment : t -> string option
  val is_matching : t -> Path.t -> bool
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
