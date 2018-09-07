module Make (YEngine : Yaks_engine.SEngine.S) : sig

  type config = Apero_net.TcpService.Config.t

  type t 

  val create : config -> YEngine.t -> t

  val start : t -> unit Lwt.t

  val stop : t -> unit Lwt.t

end
