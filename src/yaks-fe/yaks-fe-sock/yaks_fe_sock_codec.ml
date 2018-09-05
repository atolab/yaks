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
  let (k,v) = (Property.key p, Property.value p) in 
  encode_string k buf
  >>= fun buf ->
    encode_string v buf

let decode_property  buf = 
  decode_string buf 
  >>= fun (k, buf) -> 
    decode_string buf 
    >>= fun (v, buf) -> Result.ok (Property.make k v, buf)

let encode_properties = encode_seq encode_property
let decode_properties = decode_seq decode_property    

