open Yaks_core

module MainMemoryBE = struct 
  module type Config = sig    
    val id: StorageId.t 
    val kind : backend_kind 
    val encoding : Value.encoding
    val path : Path.t
  end
  module Make (C : Config) = struct 
    module SMap = Map.Make(String)
    
    let mvar_self = Lwt_mvar.create SMap.empty
    let kind = C.kind
    let path = C.path

    let get selector = 
      let%lwt self = Lwt_mvar.take mvar_self in 
      Lwt.return (self 
      |> SMap.filter (fun key _ -> Selector.is_matching selector (Path.of_string key))
      |> SMap.to_seq 
      |> List.of_seq)

    let put key value =        
    (* Stores should potentailly convert the encoding for the value. 
           For the main memory, we keep the value in its original format 
           and just convert in the front-end when required *)        
      match Path.is_prefix path (Path.of_string key) with 
      | true ->         
          let%lwt self = Lwt_mvar.take mvar_self in 
          Lwt_mvar.put mvar_self (SMap.add key value self)         
      | false -> Lwt.fail (YException (`StoreError (`Msg "Can't store key that does not prefix storage")))
  end
end 
let make_memory_be path _ =
  let module M = MainMemoryBE.Make (
    struct 
      let kind = Memory
      let id = StorageId.next_id ()
      let encoding = Value.Raw_Encoding
      let path = path
    end) in (module M : Backend)

module MainMemoryBEF = struct 
  let kind = Yaks_core.Memory
  let make (path:Path.t) (ps:Property.t list)  = make_memory_be path ps
  let name = yaks_backend_memory
end

