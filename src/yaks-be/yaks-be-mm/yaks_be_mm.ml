open Apero
open Yaks_core
open Yaks_types

module MainMemoryBE = struct 

  module type Config = sig    
    val id: BeId.t 
    val properties : properties
  end

  module Make (C : Config) (MVar : Apero.MVar) = struct 
    open Lwt.Infix
    module SMap = Map.Make(Path)

    let mvar_self = MVar.create SMap.empty

    let id = C.id
    let properties = C.properties

    let to_string = "MainMemoryBE#"^(BeId.to_string C.id)^"{"^(Properties.to_string properties)^"}"

    let get selector =       
      match Selector.as_unique_path selector with 
      | Some path ->
        MVar.read mvar_self 
        >|= (fun self -> 
            match SMap.find_opt path self with 
            | Some v -> [(path, v)]
            | None -> [])
      | None -> 
        MVar.read mvar_self 
        >|= (fun self ->
            self
            |> SMap.filter (fun path _ -> Selector.is_matching_path path selector)
            |> SMap.bindings)



    let put (path:Path.t) (value:TimedValue.t) =
      (* Stores should potentailly convert the encoding for the value. 
             For the main memory, we keep the value in its original format 
             and just convert in the front-end when required *)        
      MVar.guarded mvar_self
        (fun self -> Lwt.return (Lwt.return_unit, SMap.add path value self))        

    let try_update (tv:TimedValue.t) (d:TimedValue.t) : TimedValue.t = match Value.update tv.value d.value with
      | Ok r -> { time=tv.time; value=r }
      | Error _ -> { time=tv.time; value=tv.value }

    let update path delta =       
      MVar.guarded mvar_self
      @@ fun self -> 
      (match SMap.find_opt path self with 
        | Some v -> Lwt.return (Lwt.return_unit, SMap.add path (try_update v delta) self)        
        | None -> Lwt.return (Lwt.return_unit, SMap.add path delta self))        


    let remove path = 
      MVar.guarded mvar_self 
      @@ fun self -> Lwt.return (Lwt.return_unit, SMap.remove path self)

    let dispose () = 
      MVar.guarded mvar_self
        (fun _ -> Lwt.return (Lwt.return_unit, SMap.empty))        


    let create_storage selector props =
      Lwt.return @@ Storage.make selector props dispose get put update remove
  end
end 


module MainMemoryBEF = struct 
  let kind = Property.Backend.Value.memory

  let make id properties =
    let module M = MainMemoryBE.Make (
    struct 
      let id = id
      let properties = Properties.add Property.Backend.Key.kind Property.Backend.Value.memory properties
    end) (Apero.MVar_lwt) in (module M : Backend)

end
