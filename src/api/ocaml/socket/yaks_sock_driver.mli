type t

val create : Apero_net.Locator.t -> t Lwt.t
val destroy : t ->  unit Lwt.t
val sendmsg : Yaks_fe_sock_types.message -> t -> unit Lwt.t
val recvmsg : t -> Yaks_fe_sock_types.message Lwt.t

