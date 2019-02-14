open Yaks_core

module Make (YEngine : Yaks_engine.Engine.S) : sig

  module Config : sig 
    include module type of Apero_net.NetServiceWebSock.Config
  end  

  type t 

  val create : FeId.t -> Config.t -> YEngine.t -> t

  val start : t -> unit Lwt.t

  val stop : t -> unit Lwt.t

end
