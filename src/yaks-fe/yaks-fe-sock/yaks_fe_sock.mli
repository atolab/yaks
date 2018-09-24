module Make (YEngine : Yaks_engine.SEngine.S) : sig

  module Config : sig 
    include module type of Apero_net.NetServiceTcp.TcpConfig
  end  

  type t 

  val create : Config.t -> YEngine.t -> t

  val start : t -> unit Lwt.t

  val stop : t -> unit Lwt.t

end
