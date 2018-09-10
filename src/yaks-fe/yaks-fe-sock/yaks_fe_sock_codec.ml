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

let encode_value v = encode_string @@ Yaks_core.Value.to_string v 

let decode_value flags buf =   
  match get_encoding flags with 
  | RAW -> 
    decode_vle buf 
    >>= fun (len, buf) ->        
      IOBuf.blit_to_bytes (Vle.to_int len) buf 
      >>= fun (bs, buf) ->
        Result.ok (Yaks_core.Value.RawValue bs, buf)
  | JSON -> 
    decode_string buf 
    >>= fun (s, buf) -> 
      Result.ok (Yaks_core.Value.JSonValue s, buf)      
  | _ ->         
    Result.fail @@ `InvalidFormat `NoMsg

let decode_pair decode_fst decode_snd buf = 
  decode_fst buf 
  >>= fun (fst, buf) ->
    decode_snd buf 
    >>= fun (snd, buf) -> Result.ok ((fst, snd), buf)  

let encode_pair encode_fst encode_snd fst snd buf = 
  encode_fst fst buf >>= encode_snd snd 

let encode_selector s = encode_string (Yaks_core.Selector.to_string s)

let decode_selector buf = 
  decode_string buf 
  >>= fun (s, buf) -> 
  match (Yaks_core.Selector.of_string s) with 
  | Some s -> Result.ok (s, buf)
  | None ->       
    Result.fail  @@ `InvalidFormat (`Msg "Invalid selector format" )

let encode_path p = encode_string (Yaks_core.Path.to_string p)

let decode_paths buf = 
  decode_string buf 
  >>= fun (p, buf) -> 
  match (Yaks_core.Path.of_string p) with 
  | Some path -> Result.ok (path, buf)
  | None ->       
    Result.fail  @@ `InvalidFormat (`Msg "Invalid selector format" )

let decode_body (mid:message_id) (flags:char) (buf: IOBuf.t) = 
  match mid with 
  | OPEN -> Ok (YEmpty, buf)
  | CREATE -> 
    decode_string buf >>= fun (path, buf) -> 
    (match Yaks_core.Path.of_string path with 
     | Some p ->  Ok (YPath p, buf) 
     | None -> Result.fail (`InvalidFormat (`Msg "Invalid path syntax")))

  | DELETE ->
    (match (has_access_flag flags, has_storage_flag flags) with 
     | (true, false) -> Ok (YEmpty, buf)  
     | (false, true) -> Ok (YEmpty, buf)  
     | (false, false) -> 
       decode_string buf 
       >>= fun (sel, buf) -> 
       (match Yaks_core.Selector.of_string sel with
        | Some s ->  Ok (YSelector s, buf)  
        | None -> Result.fail (`InvalidFormat (`Msg "Invalid selector syntax"))) 
     | _ -> Result.fail `InvalidFlags)  
  
  | PUT ->
    let decode_sv = decode_pair decode_selector (decode_value flags) in 
    let decode_svs = decode_seq decode_sv in 
    decode_svs buf 
    >>= fun (svs, buf) -> Result.ok (YKeyValueList svs, buf)

  | PATCH ->
    decode_selector buf 
    >>= fun (s, buf) -> 
      decode_value flags buf 
      >>= fun (v, buf) -> Result.ok (YKeyDeltaValue (s, v), buf)

  | GET | SUB -> decode_selector buf >>= fun (s, buf) -> Result.ok (YSelector s, buf)  
  | UNSUB -> decode_string buf >>= fun (s, buf) -> Ok (YSubscription s, buf)    
  | EVAL -> Result.fail `NotImplemented
  (* These are the messages that the service send but do not expect to receive. 
     If any of this message is received the client is considered malfunctioning or 
     malicious and the connection is immediately closed *)
  | OK | ERROR | NOTIFY | VALUE | VALUES -> Result.fail `UnexpextedMessage 



let encode_body body buf = 
  match body with 
  | YEmpty -> Ok buf
  | YPath p -> encode_string (Yaks_core.Path.to_string p) buf
  | YSelector s -> encode_string (Yaks_core.Selector.to_string s) buf
  | YSubscription s -> encode_string s buf
  | YKeyDeltaValue (s, v) -> 
    encode_selector s buf >>= encode_value v   
  | YKeyValueList  kvs -> 
    Result.fold_m (fun (s, v) buf -> encode_selector s buf >>= encode_value v) kvs buf 
  | YNotification (sid, kvs) -> 
    encode_string sid buf 
    >>= Result.fold_m (fun (k, v) buf -> encode_path k buf >>= encode_value v) kvs 
  | YErrorInfo (code) -> encode_vle code buf


let decode_message buf = 
  let open Result.Infix in 
  decode_header buf 
  >>= fun (header, buf) ->     
  decode_body header.mid  header.flags buf 
  >>= fun (body, buf) -> Result.ok ({header;  body}, buf)

let encode_message msg buf =
  encode_header msg.header buf
  >>= encode_body msg.body

