module Property : sig 
  include (module type of Apero.KeyValueF.Make (String) (String))
end 

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
end

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
end

module SubscriberId : Apero.Id.S 
module PluginId : Apero.Id.S
module KeyValue : sig 
 include (module type of Apero.KeyValueF.Make (String) (String))
end 

module Path : sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val is_prefix : t -> t -> bool
  val matches : t -> t -> bool
end

module Selector : sig

  type t          
  val of_string : string -> t
  val to_string : t -> string
  val path : t -> Path.t
  val query : t -> string option
  val fragment : t -> string option
  val is_matching : t -> Path.t -> bool
end

type value_encoding = 
  | Default_Encoding
  | String_Encoding 
  | Json_Encoding
  | Binary_Encoding
  | XML_Encoding


type error_kind = [`NoMsg | `Msg of string | `Code of int | `Pos of (string * int * int * int) | `Loc of string] [@@deriving show]  

type yerror = [
  | `UnknownStorageKind 
  | `UnavailableStorageFactory of error_kind
  | `UnkownAccessId of error_kind
  | `StoreError of error_kind
  ] [@@deriving show]

exception YException of yerror [@@deriving show]
