

module MVar : Apero.MVar
module AccessId : (module type of Apero.Uuid)
module StorageId : (module type of Apero.Uuid)
module SubscriberId : (module type of Yaks_types.SubscriberId)

type listener = (Yaks_types.Path.t * Yaks_types.Value.t) list -> unit Lwt.t
type eval_callback = Yaks_types.Path.t -> Yaks_types.Value.t