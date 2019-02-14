open Apero
open Yaks_core
open Yaks_types

module MainMemoryBE = struct 

  module type Config = sig    
    val id: BeId.t 
    val properties : properties
  end

  module Make (C : Config) = struct     
    module SMap = Map.Make(Path)

    let state = Guard.create SMap.empty

    let id = C.id
    let properties = C.properties

    let to_string = "MainMemoryBE#"^(BeId.to_string C.id)^"{"^(Properties.to_string properties)^"}"

    let get selector =       
      let self = Guard.get state in        
      Lwt.return @@ match Selector.as_unique_path selector with 
      | Some path ->
        (match SMap.find_opt path self with 
        | Some v -> [(path, v)]
        | None -> [])
      | None -> 
          self
          |> SMap.filter (fun path _ -> Selector.is_matching_path path selector)
          |> SMap.bindings



    let put (path:Path.t) (value:TimedValue.t) =
      Guard.guarded state
        @@ fun self ->
          match SMap.find_opt path self with
          | Some v -> if (TimedValue.preceeds ~first:v ~second:value)
              then Guard.return () (SMap.add path value self)
              else Guard.return () self
          | None -> Guard.return  () (SMap.add path value self)         (* TODO: manage timestamped removals ! *)

    let try_update (tv:TimedValue.t) (d:TimedValue.t) : TimedValue.t = match Value.update tv.value d.value with
      | Ok r -> { time=tv.time; value=r }
      | Error _ -> { time=tv.time; value=tv.value }

    let update path delta =       
      Guard.guarded state
      @@ fun self -> 
      match SMap.find_opt path self with 
        | Some v -> if (TimedValue.preceeds ~first:v ~second:delta)
            then Guard.return () (SMap.add path (try_update v delta) self)
            else Guard.return () self
        | None -> Guard.return () (SMap.add path delta self)          (* TODO: manage timestamped removals ! *)


    let remove path = 
      Guard.guarded state 
      @@ fun self -> Guard.return () (SMap.remove path self)

    let dispose () = 
      Guard.guarded state
        (fun _ -> Guard.return () (SMap.empty))        


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
    end) in (module M : Backend)

end
