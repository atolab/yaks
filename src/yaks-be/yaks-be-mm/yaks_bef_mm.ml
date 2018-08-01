open Yaks_core

module MainMemoryBE = struct 
  module type Config = sig    
    val id: StorageId.t 
    val kind : backend_kind 
    val encoding : Value.encoding
    val path : Path.t
  end
  module Make (C : Config) (MVar : Apero.MVar) = struct 
    open Lwt.Infix
    module SMap = Map.Make(String)
    
    let mvar_self = MVar.create SMap.empty
    let kind = C.kind
    let path = C.path

    let get selector =       
      match Selector.key selector with 
      | Some key -> 
        MVar.read mvar_self 
        >|= (fun self -> 
          match SMap.find_opt key self with 
          | Some v -> [(key, v)]
          | None -> [])
      | None -> 
        MVar.read mvar_self 
        >|= (fun self ->
          self
          |> SMap.filter (fun key _ -> Selector.match_string selector key)
          |> SMap.bindings)
      
      

    let put (selector:Selector.t) (value:Value.t) =        
    (* Stores should potentailly convert the encoding for the value. 
           For the main memory, we keep the value in its original format 
           and just convert in the front-end when required *)        
      match Selector.key selector with 
      | Some key -> 
        MVar.guarded mvar_self
        (fun self -> Lwt.return (Lwt.return_unit, SMap.add key value self))        
      | None -> 
        MVar.guarded mvar_self
        (fun self -> 
          let matches = SMap.filter (fun key _ -> Selector.match_string selector key) self in 
          let self' = SMap.fold (fun k _ s -> SMap.add k value s) matches self  in 
          Lwt.return (Lwt.return_unit, self'))
        
  end
end 
let make_memory_be path _ =
  let module M = MainMemoryBE.Make (
    struct 
      let kind = Memory
      let id = StorageId.next_id ()
      let encoding = Value.Raw_Encoding
      let path = path
    end) (Apero.MVar_lwt) in (module M : Backend)

module MainMemoryBEF = struct 
  let kind = Yaks_core.Memory
  let make (path:Path.t) (ps:Property.t list)  = make_memory_be path ps
  let name = yaks_backend_memory
end

