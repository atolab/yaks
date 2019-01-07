open Apero
open Yaks_types

module Yid = Uuid

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

