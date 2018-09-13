open Yaks_core
open Yaks_types

module MainMemoryBE = struct 

  module type Config = sig    
    val id: Apero.Uuid.t 
    val properties : properties
  end

  module Make (C : Config) (MVar : Apero.MVar) = struct 
    open Lwt.Infix
    module SMap = Map.Make(Path)

    let mvar_self = MVar.create SMap.empty

    let properties = C.properties

    let to_string = "MainMemoryBE#"^(Apero.Uuid.to_string C.id)^"{"^(string_of_properties properties)^"}"

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
            |> SMap.filter (fun path _ -> Selector.is_matching path selector)
            |> SMap.bindings)



    let put (selector:Selector.t) (value:Value.t) =        
      (* Stores should potentailly convert the encoding for the value. 
             For the main memory, we keep the value in its original format 
             and just convert in the front-end when required *)        
      match Selector.as_unique_path selector with 
      | Some path -> 
        MVar.guarded mvar_self
          (fun self -> Lwt.return (Lwt.return_unit, SMap.add path value self))        
      | None -> 
        MVar.guarded mvar_self
          (fun self -> 
             let matches = SMap.filter (fun path _ -> Selector.is_matching path selector) self in 
             let self' = SMap.union (fun _ _ _ -> Some value) matches self in
             Lwt.return (Lwt.return_unit, self'))

    let try_update v d = match Value.update v d with
      | Ok r -> r
      | Error _ -> v 

    let put_delta selector delta =       
      match Selector.as_unique_path selector with 
      | Some path -> 
        MVar.guarded mvar_self
        @@ fun self -> 
        (match SMap.find_opt path self with 
         | Some v -> Lwt.return (Lwt.return_unit, SMap.add path (try_update v delta) self)        
         | None -> Lwt.return (Lwt.return_unit, SMap.add path delta self))        
      | None -> 
        MVar.guarded mvar_self
        @@ fun self -> 
        let matches = SMap.filter (fun path _ -> Selector.is_matching path selector) self in
        let self' = SMap.union (fun _ v _ -> Some (try_update v delta)) matches self in
        Lwt.return (Lwt.return_unit, self')


    let remove selector = 
      match Selector.as_unique_path selector with 
      | Some path -> 
        MVar.guarded mvar_self 
        @@ fun self -> Lwt.return (Lwt.return_unit, SMap.remove path self)
      | None -> 
        MVar.guarded mvar_self 
        @@ fun self -> 
        let matches = SMap.filter (fun path _ -> Selector.is_matching path selector) self in
        let self' = SMap.union (fun _ _ _ -> None) matches self in
        Lwt.return (Lwt.return_unit, self')              

    let dispose () = 
      MVar.guarded mvar_self
        (fun _ -> Lwt.return (Lwt.return_unit, SMap.empty))        


    let create_storage ?alias path props =
      Storage.make ?alias path props dispose get put put_delta remove
  end
end 


let make_memory_be props =
  let module M = MainMemoryBE.Make (
    struct 
      let id = Apero.Uuid.make ()
      let properties = add_property Property.Backend.Key.kind Property.Backend.Value.memory props
    end) (Apero.MVar_lwt) in (module M : Backend)

module MainMemoryBEF = struct 
  let make (props:properties) = make_memory_be props
  let name = Property.Backend.Value.memory
end

