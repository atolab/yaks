
module MVar = Apero.MVar_lwt
module AccessId = Apero.Uuid
module StorageId =  Apero.Uuid
module SubscriberId = Yaks_types.SubscriberId

type listener = (Yaks_types.Path.t * Yaks_types.Value.t) list -> unit Lwt.t
type eval_callback = Yaks_types.Path.t -> Yaks_types.Value.t