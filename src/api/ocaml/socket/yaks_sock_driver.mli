type t

val create : Apero_net.Locator.t -> t Lwt.t
val destroy : t ->  unit Lwt.t
val process : Yaks_fe_sock_types.message -> t -> Yaks_fe_sock_types.message Lwt.t

