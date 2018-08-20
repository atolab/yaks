module Make (YEngine : Yaks_engine.SEngine.S) : sig

  type config = { port : int }

  type t

  val create : config -> YEngine.t -> t

  val start : t -> unit Lwt.t

  val stop : t -> unit

end
