open Apero
open Yaks_core 
open Yaks_fe_sock_codes
open Yaks_fe_sock_types
open Lwt.Infix

module Processor = struct 
  module type S = sig 

    module YEngine : Yaks_engine.SEngine.S 

    val process_open : YEngine.t -> message -> message Lwt.t
    val process_create_access : YEngine.t -> message -> Yaks_types.Path.t -> message Lwt.t
    val process_create_storage : YEngine.t -> message -> Yaks_types.Path.t -> message Lwt.t
    val process_create : YEngine.t -> message -> message Lwt.t
    val process_delete_access : YEngine.t -> message -> message Lwt.t
    val process_delete_storage : YEngine.t -> message -> message Lwt.t
    val process_delete_tuple : YEngine.t -> message -> Yaks_types.Selector.t -> message Lwt.t
    val process_delete : YEngine.t -> message -> message Lwt.t
    val process_put : YEngine.t -> message -> message Lwt.t
    val process_get : YEngine.t -> message -> message Lwt.t
    val process_sub : YEngine.t -> message -> message Lwt.t
    val process_unsub : YEngine.t -> message -> message Lwt.t
    val process_eval : YEngine.t -> message -> message Lwt.t
    val process_error :  message -> error_code -> message Lwt.t
  end

  module Make (Engine : Yaks_engine.SEngine.S ) = struct 
    
    module YEngine = Engine  

    let reply_with_ok msg ps =     
      let header = make_header OK [] msg.header.corr_id ps in 
      make_message header YEmpty 

    let reply_with_values msg vs =     
      let header = make_header OK [] msg.header.corr_id Property.Map.empty in 
      make_message header (YKeyValueList vs)

    let reply_with_error msg code =   
      let header = make_header ERROR [] msg.header.corr_id Property.Map.empty in 
      make_message header  @@ YErrorInfo (Vle.of_int @@ error_code_to_int code)

    let process_open (* engine *) _ msg = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "FES: processing OPEN") in  
      Lwt.return @@ reply_with_ok msg Property.Map.empty

    let process_create_access engine msg path = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "FES: processing CREATE:Access") in
      let open Option.Infix in 
      let header = msg.header in     
      let cache_size_opt = decode_property_value Int64.of_string Property.Access.Key.cache_size header.properties in 
      let alias_opt = get_property Property.Access.Key.alias header.properties in 

      let accest_opt = cache_size_opt >>= fun cache_size ->
        Some (YEngine.create_access engine ?alias:alias_opt path cache_size) in 
      match accest_opt with 
      | Some access -> 
        Lwt.bind access 
        @@ fun a -> 
        let access_id = Access.Id.to_string (Access.id a) in
        let props = Property.Map.add Property.Access.Key.id access_id Property.Map.empty in
        Lwt.return @@ reply_with_ok msg props
      | None -> Lwt.return @@ reply_with_error msg BAD_REQUEST 

    let process_create_storage engine msg path = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "FES: processing CREATE:Storage") in
      let ps = msg.header.properties in 
      let alias_opt = get_property Property.Storage.Key.alias ps in    
      let%lwt storage = YEngine.create_storage engine ?alias:alias_opt path ps in 
      let storage_id = Storage.Id.to_string (Storage.id storage) in     
      let ps = Property.Map.singleton Property.Storage.Key.id storage_id in 
      Lwt.return @@ reply_with_ok msg ps

    let process_create engine msg =       
      match get_path_payload msg with 
      | Some path ->       
        if has_access_flag msg.header.flags then 
          process_create_access engine msg path
        else if has_storage_flag msg.header.flags then 
          process_create_storage engine msg path
        else 
          Lwt.return @@ reply_with_error msg BAD_REQUEST 
      | None -> Lwt.return @@  reply_with_error msg BAD_REQUEST

    let process_delete_access engine msg = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "FES: processing DELETE:Access") in
      let ps = msg.header.properties in 
      let aid = (get_property Property.Access.Key.id ps) in    
      match Option.bind aid Access.Id.of_string   with 
      | Some access_id -> 
        YEngine.dispose_access engine access_id 
        >>= fun () -> Lwt.return @@ reply_with_ok msg Property.Map.empty
      | None -> Lwt.return @@ reply_with_error msg BAD_REQUEST 

    let process_delete_storage engine msg = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "FES: processing DELETE:Storage") in
      let ps = msg.header.properties in 
      let sid = (get_property Property.Storage.Key.id ps) in    
      match Option.bind sid Storage.Id.of_string   with 
      | Some storage_id ->       
        YEngine.dispose_storage engine storage_id 
        >>= fun () -> Lwt.return @@ reply_with_ok msg Property.Map.empty
      | None -> Lwt.return @@ reply_with_error msg BAD_REQUEST 

    let process_delete_tuple engine msg selector = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "FES: processing DELETE:Tuple") in
      let ps = msg.header.properties in 
      let aid = (get_property Property.Access.Key.id ps) in    
      match Option.bind aid Access.Id.of_string   with 
      | Some access_id -> 
        YEngine.remove engine access_id selector 
        >>= fun () -> Lwt.return @@ reply_with_ok msg Property.Map.empty 
      | None -> Lwt.return @@ reply_with_error msg BAD_REQUEST 

    let process_delete engine msg = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "FES: processing DELETE") in 
      match (has_access_flag msg.header.flags, has_storage_flag msg.header.flags) with
      | (true, _) ->     
        process_delete_access engine msg
      | (_, true) -> 
        process_delete_storage engine msg
      | _ -> 
        (match get_selector_payload msg with 
         | Some selector -> process_delete_tuple engine msg selector 
         | None -> Lwt.return @@  reply_with_error msg BAD_REQUEST)

    let process_put engine msg = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "FES: processing PUT") in 
      let ps = msg.header.properties in       
      let params = Option.bind (get_key_value_list_payload msg) 
        @@ fun kvs ->       
        let id = Option.bind (get_property Property.Access.Key.id ps) Access.Id.of_string in 
        Option.bind id 
        @@ fun aid -> Some (kvs, aid) 
      in match params with 
      | Some (kvs, aid) ->         
        Lwt.join 
          @@ List.map (fun (k,v) -> 
                let%lwt _ = Logs_lwt.debug (fun m -> m "FES: PUT %s" (Yaks_core.Selector.to_string k)) in 
                YEngine.put engine aid k v)  
              kvs       
        >>= fun () -> Lwt.return @@ reply_with_ok msg Property.Map.empty
      | None -> Lwt.return @@ reply_with_error msg BAD_REQUEST


    let process_get engine msg =
      let%lwt _ = Logs_lwt.debug (fun m -> m "FES: processing GET") in
      let open Apero.Infix in 
      let ps = msg.header.properties in 
      let params = Option.bind (decode_property_value (Option.get <.> Access.Id.of_string) Property.Access.Key.id ps) 
        @@ fun aid -> 
        Option.bind (get_selector_payload msg)
        @@ fun s -> Some (aid, s) 
      in     
      match params with 
      | Some (aid, s) -> 
        YEngine.get engine aid s 
        >>= fun kvs -> 
        let vs = List.map (fun (k,v) -> (Option.get (Selector.of_string k), v)) kvs  in
        Lwt.return @@ reply_with_values msg vs
      | None -> Lwt.return @@ reply_with_error msg BAD_REQUEST

    let process_sub engine msg = let _ = engine in Lwt.return @@ reply_with_ok msg Property.Map.empty
    let process_unsub engine msg = let _ = engine in Lwt.return @@ reply_with_ok msg Property.Map.empty
    let process_eval engine msg = let _ = engine in Lwt.return @@ reply_with_ok msg Property.Map.empty

    let process_error msg code = Lwt.return @@ reply_with_error msg code 
  end
end 

