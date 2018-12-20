open Yaks_core

module Make (YEngine : Yaks_engine.Engine.S) : sig

  type config = { id : FeId.t; port : int }

  type t

  val create : config -> YEngine.t -> t

  val start : t -> unit Lwt.t

  val stop : t -> unit Lwt.t

end
