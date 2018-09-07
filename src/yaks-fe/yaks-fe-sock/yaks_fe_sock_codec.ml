open Apero
open Apero.Result.Infix
open Yaks_fe_sock_types

let encode_header h buf =
  let _ = Logs_lwt.debug (fun m -> m "Yaks_fe_sock - encoding header") in 
  let id = char_of_int @@ message_id_to_int h.mid in   
  IOBuf.put_char id buf 
  >>= fun buf -> 
    IOBuf.put_char h.flags buf 
    >>= fun buf ->
      encode_vle h.corr_id buf 
      

let decode_header =  
  let make_header id flags corr_id = 
    {mid = Option.get @@ int_to_message_id (int_of_char id); flags; corr_id }
  in
  read3_spec 
    (let _ = Logs_lwt.debug (fun m -> m "Yaks_fe_sock - encoding header") in ()) 
    IOBuf.get_char
    IOBuf.get_char    
    decode_vle
    make_header

let encode_property p buf =
  let open Yaks_core in 
  let (k,v) = (Property.key p, Property.value p) in 
  encode_string k buf
  >>= fun buf ->
    encode_string v buf

let decode_property  buf = 
  let open Yaks_core in 
  decode_string buf 
  >>= fun (k, buf) -> 
    decode_string buf 
    >>= fun (v, buf) -> Result.ok (Property.make k v, buf)

let encode_properties = encode_seq encode_property
let decode_properties = decode_seq decode_property    


let decode_body (mid:message_id)  (buf: IOBuf.t) = 
  match mid with 
  | OPEN -> Ok (Empty, buf)
  | CREATE | DELETE -> 
    decode_string buf >>= fun (path, buf) -> Ok (Path path, buf)  
  | PUT -> 
    decode_string buf 
    >>= fun (key, buf) -> decode_bytes buf 
    >>= fun (value, buf) -> Ok (KeyValue (key, value), buf)
  | PATCH -> 
    decode_string buf 
    >>= fun (key, buf) -> decode_bytes buf 
    >>= fun (value, buf) -> Ok (KeyDeltaValue (key, value), buf)
  | GET -> decode_string buf >>= fun (s, buf) -> Ok (Selector s, buf)
  | SUB -> decode_string buf >>= fun (s, buf) -> Ok (Selector s, buf)
  | UNSUB -> decode_string buf >>= fun (s, buf) -> Ok (Subscription s, buf)
  | OK -> Ok (Empty, buf)
  | ERROR -> 
    decode_vle buf 
    >>= fun (corr, buf) -> IOBuf.get_char buf
    >>= fun (e, buf) -> Ok (ErrorInfo (corr, e), buf)
  | EVAL -> Result.fail `NotImplemented

let encode_body body buf = 
  match body with 
  | Empty -> Ok buf
  | Path p -> encode_string p buf
  | Selector s -> encode_string s buf
  | Subscription s -> encode_string s buf
  | KeyValue (k, v) -> 
    encode_string k buf >>= encode_bytes v 
  | KeyDeltaValue (k, v) -> 
    encode_string k buf >>= encode_bytes v 
  | KeyValueList  kvs -> 
    Result.fold_m (fun (k, v) buf -> encode_string k buf >>= encode_bytes v) kvs buf 
  | Notification (sid, kvs) -> 
    encode_string sid buf 
    >>= Result.fold_m (fun (k, v) buf -> encode_string k buf >>= encode_bytes v) kvs 
  | ErrorInfo (corr, code) -> encode_vle corr buf >>= IOBuf.put_char code 
  

let decode_message buf = 
  let open Result.Infix in 
  decode_header buf 
  >>= fun (header, buf) -> 
    (if has_property_flag header then decode_properties buf 
    else Ok ([], buf))
    >>= fun (properties, buf) -> decode_body header.mid buf 
      >>= fun (body, buf) -> Ok({header; properties; body}, buf)
    
let encode_message msg buf =
  encode_header msg.header buf
  >>= encode_properties msg.properties
  >>= encode_body msg.body