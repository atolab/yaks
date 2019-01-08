open Apero
open Apero_time
open Yaks_types

module Yid = Uuid

let my_yid = Yid.make ()
(* NOTE: the unique identifier of the Yaks service is instanciated here to be used below for HLC module creation *)

module FeId = Id.Make(
  struct
   include String
   let of_string s = s
   let to_string s = s
  end)
(** Frontend id *)
module SessionId = Id.Make(
  struct
   include String
   let of_string s = s
   let to_string s = s
  end)
(** session id *)
module WsId = NumId.Make(Int32)
(** workspace id*)
module SubscriberId = NumId.Make(Int32)
(** subscriber id*)

module BeId = Id.Make(
  struct
   include String
   let of_string s = s
   let to_string s = s
  end)
(** backend id *)
module  StorageId = Id.Make(
  struct
   include String
   let of_string s = s
   let to_string s = s
  end)
(** storage id *)


module ClientId = struct

  type t = {feid:FeId.t; sid:SessionId.t}

  let compare t t' = match FeId.compare t.feid t'.feid with
    | 0 -> SessionId.compare t.sid t'.sid
    | x -> x

  let equal t t' = (FeId.equal t.feid t'.feid) && (SessionId.equal t.sid t'.sid)

  let to_string t = (FeId.to_string t.feid)^"/"^(SessionId.to_string t.sid)

end


type notify_subscriber =  SubscriberId.t -> fallback:(SubscriberId.t -> unit Lwt.t) -> (Path.t * Value.t) list -> unit Lwt.t

type eval_function = Path.t -> Selector.t -> fallback:(Path.t -> Value.t Lwt.t) -> Value.t Lwt.t


let hlc_counter_size = 8
let hlc_max_drift = 0.1
module HLC = (val hlc_create my_yid hlc_counter_size hlc_max_drift (module Clock_unix): Apero_time.HLC)

type timed_value = { time:HLC.Timestamp.t; value:Value.t }


let encode_timed_value tv buf = 
  let open Apero.Result.Infix in
  HLC.Timestamp.encode tv.time buf >>= fun buf ->
  Yaks_fe_sock_codec.encode_value tv.value buf

let decode_timed_value buf =
  let open Apero.Result.Infix in
  HLC.Timestamp.decode buf >>= fun (time, buf) ->
  Yaks_fe_sock_codec.decode_value buf >>= fun (value, buf) ->
  Result.ok ({time; value}, buf)
