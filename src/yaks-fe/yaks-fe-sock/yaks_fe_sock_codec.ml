open Apero
open Apero.Result.Infix
open Yaks_fe_sock_codes
open Yaks_fe_sock_types

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
let decode_properties buf = 
  decode_seq decode_property buf   

let encode_header h buf =  
  let id = char_of_int @@ message_id_to_int h.mid in   
  IOBuf.put_char id buf 
  >>= fun buf -> 
    IOBuf.put_char h.flags buf 
    >>= fun buf ->
      encode_vle h.corr_id buf 
      

let decode_header buf =   
  let open Yaks_core in 
  IOBuf.get_char buf 
  >>= fun (id, buf) -> IOBuf.get_char buf 
    >>= fun (flags, buf) -> decode_vle buf 
      >>= fun (corr_id, buf) -> 
        (if has_property_flag flags then decode_properties buf
        else 
          let ps : Property.t list = [] in Result.ok (ps, buf) )
        >>= fun (properties, buf) -> 
          match int_to_message_id (int_of_char id) with 
          | Some mid -> Result.ok ({mid; flags; corr_id; properties}, buf)
          | None -> Result.fail `UnknownMessageId 

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
  | EVAL -> Result.fail `NotImplemented
  (* These are the messages that the service send but do not expect to receive. 
     If any of this message is received the client is considered malfunctioning or 
     malicious and the connection is immediately closed *)
  | OK | ERROR | NOTIFY | VALUE | VALUES-> Result.fail `UnexpextedMessage

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
  | ErrorInfo (code) -> encode_vle code buf
  

let decode_message buf = 
  let open Result.Infix in 
  decode_header buf 
  >>= fun (header, buf) ->     
      decode_body header.mid buf 
      >>= fun (body, buf) -> Result.ok ({header;  body}, buf)
    
let encode_message msg buf =
  encode_header msg.header buf
  >>= encode_body msg.body