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
  val of_string : ?is_absolute:bool -> string -> t
  (** [of_string s] returns [s] as a Path if it's valid. Otherwise it raises a [YException] *)
  val of_string_opt : ?is_absolute:bool -> string -> t option
  (** [of_string_opt s] returns [Some p] if [s] is a valid path. Otherwise it raises a [YException] *)
  val to_string : t -> string
  val is_prefix : affix:t -> t -> bool
  (** [is_prefix prefix path] test if [prefix] is a prefix of [path] *)
  (* val matches : t -> t -> bool *)
  val compare : t -> t-> int
end [@@deriving show]

module Selector : sig
  type t          
  val of_string : ?is_absolute:bool -> string -> t
  val of_string_opt : ?is_absolute:bool -> string -> t option
  val to_string : t -> string
  val path : t -> string
  (** [path s] returns the path part of the Selector [s]. I.e. the part before any '?' character. *)
  val query : t -> string option
  (** [query s] returns the query part of the Selector [s].
      I.e. the part after the first '?' character and before the fist '#' character, or an empty string if no '?' is found. *)
  val fragment : t -> string option
  (** [fragment s] returns the fragment part of the Selector [s].
      I.e. the part after the first '#', or an empty string if no '#' is found. *)
  val is_unique_path : t -> bool
  (** [is_unique_path s] returns true if the path part of Selector [s] doesn't contains any wildcard ('*') *)
  val as_unique_path : t -> Path.t option
  (** [as_unique_path s] returns Some [path s] if [is_unique_path s] returns true, None otherwise *)
  val is_matching_path : Path.t -> t -> bool
    (** [is_matching_path p s] returns true if the Selector [s] matches the Path [p] *)
  val is_prefixed_by_path : Path.t -> t -> bool
    (** [is_prefixed_by_path p s] returns true if the Path [p] is a prefix of the Selector [s]
        (i.e. it exists at least 1 sub-path of [p] that matches the Selector [s]) *)
  val remove_prefix : Path.t -> t -> t
(** [remove_prefix p s] returns the remaining part of [s] when the prefix [p] has been removed
    (assuming [p] is a matching prefix of [s]). *)
end [@@deriving show]

type error_info = [`NoMsg | `Msg of string | `Code of int | `Pos of (string * int * int * int) | `Loc of string] [@@deriving show]  

type yerror = [
  | `InvalidBackendProperty of error_info
  | `InsufficientStorage
  | `InvalidPath of error_info
  | `Forbidden of error_info
  | `InvalidParameters
  | `ConflictingStorage of error_info
  | `NoCompatibleBackend of error_info
  | `UnknownStorage of error_info
  | `UnknownAccess of error_info
  | `UnknownStorageKind 
  | `UnavailableStorageFactory of error_info
  | `UnkownAccessId of error_info
  | `StoreError of error_info
  | `UnauthorizedAccess of error_info
  | `UnsupportedTranscoding of error_info
  | `UnsupportedOperation
  | `InternalError of error_info
] [@@deriving show]


exception YException of yerror [@@deriving show]

module Value : sig 
  type encoding = 
    | Raw_Encoding
    | String_Encoding 
    | Json_Encoding
    | Sql_Encoding  

  type sql_row = string list
  type sql_column_names = string list

  type t  = 
    | RawValue of Lwt_bytes.t 
    | StringValue of string
    | JSonValue of string
    | SqlValue of (sql_row * sql_column_names option)


  val update : t -> t -> (t, yerror) Apero.Result.t
  val encoding : t -> encoding
  val encoding_to_string : encoding -> string
  val encoding_of_string : string -> encoding
  val transcode : t -> encoding -> (t, yerror) Apero.Result.t   
  val of_string : string -> encoding -> (t, yerror) Apero.Result.t 
  val to_string : t -> string  

end
