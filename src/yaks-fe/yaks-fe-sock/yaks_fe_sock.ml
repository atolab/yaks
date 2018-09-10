open Apero
open Apero_net
open Lwt.Infix
open Yaks_core
open Yaks_fe_sock_codes
open Yaks_fe_sock_types
open Yaks_fe_sock_codec

module Make (YEngine : Yaks_engine.SEngine.S) = struct 
  module SocketFE = TcpService.Make (MVar_lwt)

  module Config = Apero_net.TcpService.Config
  
  type t = SocketFE.t * SocketFE.io_service
  
  let reader sock  = 
    let lbuf = IOBuf.create 16 in 
    let%lwt len = Net.read_vle sock lbuf in          
    let buf = IOBuf.create (Vle.to_int len) in 
    let%lwt _ = Net.read sock buf in     
    match decode_message buf with 
    | Ok (msg, _) -> Lwt.return msg
    | Error e -> Lwt.fail @@ Exception e

  let writer buf sock msg = 
    match encode_message msg buf with 
    | Ok buf ->  
      let lbuf = IOBuf.create 16 in 
      let fbuf = (IOBuf.flip buf) in       
      (match encode_vle (Vle.of_int @@ IOBuf.limit fbuf) lbuf with 
      | Ok lbuf -> 
        Net.send_vec sock [IOBuf.flip lbuf; fbuf]
      | Error e -> Lwt.fail @@ Exception e )     
    | Error e -> Lwt.fail @@ Exception e 

  let reply_with_ok msg ps =     
    let header = make_header OK [] msg.header.corr_id ps in 
     make_message header YEmpty 
  
  let reply_with_values msg vs =     
    let header = make_header OK [] msg.header.corr_id [] in 
     make_message header (YKeyValueList vs)

  let reply_with_error msg code =   
    let header = make_header ERROR [] msg.header.corr_id [] in 
     make_message header  @@ YErrorInfo (Vle.of_int @@ error_code_to_int code)

  let process_open (* engine *) _ msg = Lwt.return @@ reply_with_ok msg []
  
  let process_create_access engine msg path = 
    let open Option.Infix in 
    let header = msg.header in     
    let cache_size_opt = decode_property_value Int64.of_string Property.Access.Key.cache_size header.properties in 
    let alias_opt = get_property Property.Access.Key.alias header.properties >|= fun (_,v) -> v in 

    let accest_opt = cache_size_opt >>= fun cache_size ->
          Some (YEngine.create_access engine ?alias:alias_opt path cache_size) in 
    match accest_opt with 
    | Some access -> 
      Lwt.bind access 
        @@ fun a -> 
          let access_id = Access.Id.to_string (Access.id a) in
          let p = Property.make Property.Access.Key.id access_id in                     
          Lwt.return @@ reply_with_ok msg [p]
    | None -> Lwt.return @@ reply_with_error msg BAD_REQUEST 
    
  let process_create_storage engine msg path = 
    let ps = msg.header.properties in 
    let alias_opt = 
      let open Option.Infix in 
      get_property Property.Storage.Key.alias ps >|= (fun (_, v) -> v)
    in
    
    let%lwt storage = YEngine.create_storage engine ?alias:alias_opt path ps in 
    let storage_id = Storage.Id.to_string (Storage.id storage) in 
    let p = Property.make Property.Storage.Key.id storage_id in 
    Lwt.return @@ reply_with_ok msg [p]
  
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
    let ps = msg.header.properties in 
    let aid = (get_property_value Property.Access.Key.id ps) in    
    match Option.bind aid Access.Id.of_string   with 
    | Some access_id -> 
      YEngine.dispose_access engine access_id 
      >>= fun () -> Lwt.return @@ reply_with_ok msg []
    | None -> Lwt.return @@ reply_with_error msg BAD_REQUEST 
  
  let process_delete_storage engine msg = 
    let ps = msg.header.properties in 
    let sid = (get_property_value Property.Storage.Key.id ps) in    
    match Option.bind sid Storage.Id.of_string   with 
    | Some storage_id -> 
      YEngine.dispose_storage engine storage_id 
      >>= fun () -> Lwt.return @@ reply_with_ok msg []
    | None -> Lwt.return @@ reply_with_error msg BAD_REQUEST 
  
  let process_delete_tuple engine msg selector = 
    let ps = msg.header.properties in 
    let aid = (get_property_value Property.Access.Key.id ps) in    
    match Option.bind aid Access.Id.of_string   with 
    | Some access_id -> 
      YEngine.remove engine access_id selector 
      >>= fun () -> Lwt.return @@ reply_with_ok msg [] 
    | None -> Lwt.return @@ reply_with_error msg BAD_REQUEST 

  let process_delete engine msg = 
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
    let ps = msg.header.properties in       
    let params = Option.bind (get_key_value_list_payload msg) 
    @@ fun kvs ->       
        let id = Option.bind (get_property_value Property.Access.Key.id ps) Access.Id.of_string in 
        Option.bind id 
        @@ fun aid -> Some (kvs, aid) 
    in match params with 
    | Some (kvs, aid) -> 
      Lwt.join @@ List.map (fun (k,v) -> YEngine.put engine aid k v)  kvs       
      >>= fun () -> Lwt.return @@ reply_with_ok msg [] 
    | None -> Lwt.return @@ reply_with_error msg BAD_REQUEST
              

  let process_get engine msg =
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

  let process_sub engine msg = let _ = engine in Lwt.return @@ reply_with_ok msg []
  let process_unsub engine msg = let _ = engine in Lwt.return @@ reply_with_ok msg []
  let process_eval engine msg = let _ = engine in Lwt.return @@ reply_with_ok msg []

  let dispatch_message engine msg = 
    match msg.header.mid with 
    | OPEN -> process_open engine msg 
    | CREATE -> process_create engine msg
    | DELETE -> process_delete engine msg
    | PUT -> process_put engine msg
    | GET -> process_get engine msg 
    | SUB -> process_sub engine msg 
    | UNSUB -> process_unsub engine msg
    | EVAL -> process_eval engine msg    
    | _ -> Lwt.return @@ reply_with_error msg BAD_REQUEST

    

  let fe_service config  dispatcher sock = 
    let buf = IOBuf.create (Config.buf_size config) in 
    let mwriter = writer buf sock in 
    fun () -> 
      reader sock >>= dispatcher >>= mwriter >>= fun _ -> Lwt.return_unit

  let create (conf : Config.t) (engine: YEngine.t) = 
    let svc = SocketFE.create conf in 
    let dispatcher = dispatch_message engine in 
    let io_svc = fe_service conf dispatcher in 
    (svc, io_svc)

  let start (svc, iosvc) = 

    let _ = Logs_lwt.debug (fun m -> m "[FES] Sock-FE starting TCP/IP server") in
    SocketFE.start svc iosvc    

  let stop (svc, _) = SocketFE.stop svc

end


