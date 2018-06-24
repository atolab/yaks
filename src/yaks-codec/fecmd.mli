(* Auto-generated from "fecmd.atd" *)
              [@@@ocaml.warning "-27-32-35-39"]

type tuple = { key: string; value: string }

type storage_info = { id: string option; path: string }

type storage_control = [
    `Create of storage_info
  | `Close of string
  | `Dispose of string
]

type access_operation = [
    `Put of tuple
  | `Get of string
  | `DPut of tuple
  | `Sub of string
  | `Unsub of string
]

type access_info = { id: string option; path: string; cache_size: int }

type access_control = [
    `Create of access_info
  | `Close of string
  | `Dispose of string
]

type request = [
    `SCtrl of storage_control
  | `ACtrl of access_control
  | `AOp of access_operation
]

type notification = { sid: int; elems: tuple list }

type reply = [
    `Ok
  | `Error of int
  | `Values of tuple list
  | `Notice of notification
  | `SubId of int
]

type message = { mid: int; msg: [ `Request of request | `Reply of reply ] }

type access_operator = { id: string; op: access_operation }

(* Writers for type tuple *)

val tuple_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!tuple}.
      Readers may support more than just this tag. *)

val write_untagged_tuple :
  Bi_outbuf.t -> tuple -> unit
  (** Output an untagged biniou value of type {!tuple}. *)

val write_tuple :
  Bi_outbuf.t -> tuple -> unit
  (** Output a biniou value of type {!tuple}. *)

val string_of_tuple :
  ?len:int -> tuple -> string
  (** Serialize a value of type {!tuple} into
      a biniou string. *)

(* Readers for type tuple *)

val get_tuple_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> tuple)
  (** Return a function that reads an untagged
      biniou value of type {!tuple}. *)

val read_tuple :
  Bi_inbuf.t -> tuple
  (** Input a tagged biniou value of type {!tuple}. *)

val tuple_of_string :
  ?pos:int -> string -> tuple
  (** Deserialize a biniou value of type {!tuple}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

val create_tuple :
  key: string ->
  value: string ->
  unit -> tuple
  (** Create a record of type {!tuple}. *)


(* Writers for type storage_info *)

val storage_info_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!storage_info}.
      Readers may support more than just this tag. *)

val write_untagged_storage_info :
  Bi_outbuf.t -> storage_info -> unit
  (** Output an untagged biniou value of type {!storage_info}. *)

val write_storage_info :
  Bi_outbuf.t -> storage_info -> unit
  (** Output a biniou value of type {!storage_info}. *)

val string_of_storage_info :
  ?len:int -> storage_info -> string
  (** Serialize a value of type {!storage_info} into
      a biniou string. *)

(* Readers for type storage_info *)

val get_storage_info_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> storage_info)
  (** Return a function that reads an untagged
      biniou value of type {!storage_info}. *)

val read_storage_info :
  Bi_inbuf.t -> storage_info
  (** Input a tagged biniou value of type {!storage_info}. *)

val storage_info_of_string :
  ?pos:int -> string -> storage_info
  (** Deserialize a biniou value of type {!storage_info}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

val create_storage_info :
  id: string option ->
  path: string ->
  unit -> storage_info
  (** Create a record of type {!storage_info}. *)


(* Writers for type storage_control *)

val storage_control_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!storage_control}.
      Readers may support more than just this tag. *)

val write_untagged_storage_control :
  Bi_outbuf.t -> storage_control -> unit
  (** Output an untagged biniou value of type {!storage_control}. *)

val write_storage_control :
  Bi_outbuf.t -> storage_control -> unit
  (** Output a biniou value of type {!storage_control}. *)

val string_of_storage_control :
  ?len:int -> storage_control -> string
  (** Serialize a value of type {!storage_control} into
      a biniou string. *)

(* Readers for type storage_control *)

val get_storage_control_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> storage_control)
  (** Return a function that reads an untagged
      biniou value of type {!storage_control}. *)

val read_storage_control :
  Bi_inbuf.t -> storage_control
  (** Input a tagged biniou value of type {!storage_control}. *)

val storage_control_of_string :
  ?pos:int -> string -> storage_control
  (** Deserialize a biniou value of type {!storage_control}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type access_operation *)

val access_operation_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!access_operation}.
      Readers may support more than just this tag. *)

val write_untagged_access_operation :
  Bi_outbuf.t -> access_operation -> unit
  (** Output an untagged biniou value of type {!access_operation}. *)

val write_access_operation :
  Bi_outbuf.t -> access_operation -> unit
  (** Output a biniou value of type {!access_operation}. *)

val string_of_access_operation :
  ?len:int -> access_operation -> string
  (** Serialize a value of type {!access_operation} into
      a biniou string. *)

(* Readers for type access_operation *)

val get_access_operation_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> access_operation)
  (** Return a function that reads an untagged
      biniou value of type {!access_operation}. *)

val read_access_operation :
  Bi_inbuf.t -> access_operation
  (** Input a tagged biniou value of type {!access_operation}. *)

val access_operation_of_string :
  ?pos:int -> string -> access_operation
  (** Deserialize a biniou value of type {!access_operation}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type access_info *)

val access_info_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!access_info}.
      Readers may support more than just this tag. *)

val write_untagged_access_info :
  Bi_outbuf.t -> access_info -> unit
  (** Output an untagged biniou value of type {!access_info}. *)

val write_access_info :
  Bi_outbuf.t -> access_info -> unit
  (** Output a biniou value of type {!access_info}. *)

val string_of_access_info :
  ?len:int -> access_info -> string
  (** Serialize a value of type {!access_info} into
      a biniou string. *)

(* Readers for type access_info *)

val get_access_info_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> access_info)
  (** Return a function that reads an untagged
      biniou value of type {!access_info}. *)

val read_access_info :
  Bi_inbuf.t -> access_info
  (** Input a tagged biniou value of type {!access_info}. *)

val access_info_of_string :
  ?pos:int -> string -> access_info
  (** Deserialize a biniou value of type {!access_info}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

val create_access_info :
  id: string option ->
  path: string ->
  cache_size: int ->
  unit -> access_info
  (** Create a record of type {!access_info}. *)


(* Writers for type access_control *)

val access_control_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!access_control}.
      Readers may support more than just this tag. *)

val write_untagged_access_control :
  Bi_outbuf.t -> access_control -> unit
  (** Output an untagged biniou value of type {!access_control}. *)

val write_access_control :
  Bi_outbuf.t -> access_control -> unit
  (** Output a biniou value of type {!access_control}. *)

val string_of_access_control :
  ?len:int -> access_control -> string
  (** Serialize a value of type {!access_control} into
      a biniou string. *)

(* Readers for type access_control *)

val get_access_control_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> access_control)
  (** Return a function that reads an untagged
      biniou value of type {!access_control}. *)

val read_access_control :
  Bi_inbuf.t -> access_control
  (** Input a tagged biniou value of type {!access_control}. *)

val access_control_of_string :
  ?pos:int -> string -> access_control
  (** Deserialize a biniou value of type {!access_control}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type request *)

val request_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!request}.
      Readers may support more than just this tag. *)

val write_untagged_request :
  Bi_outbuf.t -> request -> unit
  (** Output an untagged biniou value of type {!request}. *)

val write_request :
  Bi_outbuf.t -> request -> unit
  (** Output a biniou value of type {!request}. *)

val string_of_request :
  ?len:int -> request -> string
  (** Serialize a value of type {!request} into
      a biniou string. *)

(* Readers for type request *)

val get_request_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> request)
  (** Return a function that reads an untagged
      biniou value of type {!request}. *)

val read_request :
  Bi_inbuf.t -> request
  (** Input a tagged biniou value of type {!request}. *)

val request_of_string :
  ?pos:int -> string -> request
  (** Deserialize a biniou value of type {!request}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type notification *)

val notification_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!notification}.
      Readers may support more than just this tag. *)

val write_untagged_notification :
  Bi_outbuf.t -> notification -> unit
  (** Output an untagged biniou value of type {!notification}. *)

val write_notification :
  Bi_outbuf.t -> notification -> unit
  (** Output a biniou value of type {!notification}. *)

val string_of_notification :
  ?len:int -> notification -> string
  (** Serialize a value of type {!notification} into
      a biniou string. *)

(* Readers for type notification *)

val get_notification_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> notification)
  (** Return a function that reads an untagged
      biniou value of type {!notification}. *)

val read_notification :
  Bi_inbuf.t -> notification
  (** Input a tagged biniou value of type {!notification}. *)

val notification_of_string :
  ?pos:int -> string -> notification
  (** Deserialize a biniou value of type {!notification}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

val create_notification :
  sid: int ->
  elems: tuple list ->
  unit -> notification
  (** Create a record of type {!notification}. *)


(* Writers for type reply *)

val reply_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!reply}.
      Readers may support more than just this tag. *)

val write_untagged_reply :
  Bi_outbuf.t -> reply -> unit
  (** Output an untagged biniou value of type {!reply}. *)

val write_reply :
  Bi_outbuf.t -> reply -> unit
  (** Output a biniou value of type {!reply}. *)

val string_of_reply :
  ?len:int -> reply -> string
  (** Serialize a value of type {!reply} into
      a biniou string. *)

(* Readers for type reply *)

val get_reply_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> reply)
  (** Return a function that reads an untagged
      biniou value of type {!reply}. *)

val read_reply :
  Bi_inbuf.t -> reply
  (** Input a tagged biniou value of type {!reply}. *)

val reply_of_string :
  ?pos:int -> string -> reply
  (** Deserialize a biniou value of type {!reply}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type message *)

val message_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!message}.
      Readers may support more than just this tag. *)

val write_untagged_message :
  Bi_outbuf.t -> message -> unit
  (** Output an untagged biniou value of type {!message}. *)

val write_message :
  Bi_outbuf.t -> message -> unit
  (** Output a biniou value of type {!message}. *)

val string_of_message :
  ?len:int -> message -> string
  (** Serialize a value of type {!message} into
      a biniou string. *)

(* Readers for type message *)

val get_message_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> message)
  (** Return a function that reads an untagged
      biniou value of type {!message}. *)

val read_message :
  Bi_inbuf.t -> message
  (** Input a tagged biniou value of type {!message}. *)

val message_of_string :
  ?pos:int -> string -> message
  (** Deserialize a biniou value of type {!message}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

val create_message :
  mid: int ->
  msg: [ `Request of request | `Reply of reply ] ->
  unit -> message
  (** Create a record of type {!message}. *)


(* Writers for type access_operator *)

val access_operator_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!access_operator}.
      Readers may support more than just this tag. *)

val write_untagged_access_operator :
  Bi_outbuf.t -> access_operator -> unit
  (** Output an untagged biniou value of type {!access_operator}. *)

val write_access_operator :
  Bi_outbuf.t -> access_operator -> unit
  (** Output a biniou value of type {!access_operator}. *)

val string_of_access_operator :
  ?len:int -> access_operator -> string
  (** Serialize a value of type {!access_operator} into
      a biniou string. *)

(* Readers for type access_operator *)

val get_access_operator_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> access_operator)
  (** Return a function that reads an untagged
      biniou value of type {!access_operator}. *)

val read_access_operator :
  Bi_inbuf.t -> access_operator
  (** Input a tagged biniou value of type {!access_operator}. *)

val access_operator_of_string :
  ?pos:int -> string -> access_operator
  (** Deserialize a biniou value of type {!access_operator}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

val create_access_operator :
  id: string ->
  op: access_operation ->
  unit -> access_operator
  (** Create a record of type {!access_operator}. *)


