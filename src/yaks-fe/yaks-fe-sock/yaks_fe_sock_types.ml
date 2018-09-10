open Apero
open Yaks_fe_sock_codes
(* The structure of a socket front end message is the following.

        7 6 5 4 3 2 1 0
        +-+-+-+-+-+-+-+-+  ---+
        |  MESSAGE CODE |     |
        +-+-+-+-+-+-+-+-+     |
        |X|X|X|X|X|A|S|P|     +--> Header
        +-+-+-+-+-+-+-+-+     | 
        ~   Coor. ID    ~     |
        +---------------+-----+
        ~   Properties  ~ --> Present if P = 1
        +---------------+
        ~     Body      ~ --> its structure depends on the message code
        +---------------+    

For transports that do not preserve message boundaries, the framing is done by prepending
the lenght encoded using VLE.  *)

type header = { 
  mid : message_id;
  flags : char;
  corr_id : Vle.t;  
  properties : Yaks_core.Property.t list;
}

let make_header mid (mflags: message_flags list) corr_id properties = 
  let base_flags = List.fold_left (fun a f -> a lor (message_flags_to_int f)) 0 mflags in 
  let flags = char_of_int @@ match properties with 
    | [] ->  base_flags
    | _ -> (message_flags_to_int PROPERTY) lor base_flags
  in 
  {mid; flags; corr_id; properties}

let has_property_flag flags = (int_of_char flags) land 0x01 <> 0
let has_storage_flag flags = (int_of_char flags) land 0x02 <> 0
let has_access_flag flags = (int_of_char flags) land 0x04 <> 0
  
type payload = 
  | Empty
  | Path of string
  | Selector of string
  | KeyValue of string * IOBuf.t
  | KeyDeltaValue of string * IOBuf.t
  | KeyValueList of (string * IOBuf.t) list
  | Subscription of string
  | Notification of string * ((string * IOBuf.t) list)
  | ErrorInfo of Vle.t
  

type message = {
  header: header;  
  body : payload (* This could be a variant*)
}

let make_message header body = {header; body} 

let get_empty_payload msg = 
  match msg.body with 
  | Empty -> Some ()
  | _ -> None

let get_path_payload msg = 
  match msg.body with 
  | Path p -> Some p
  | _ -> None 

let get_selector_payload msg = 
  match msg.body with 
  | Selector s -> Some s
  | _ -> None 

let get_key_value_payload msg = 
  match msg.body with  
  | KeyValue (k,v) -> Some (k,v)
  | _ -> None 

let get_key_delta_value_payload msg = 
  match msg.body with  
  | KeyDeltaValue (k,dv) -> Some (k,dv)
  | _ -> None 

let get_key_value_list_payload msg = 
  match msg.body with  
  | KeyValueList kvs -> Some kvs
  | _ -> None 

let get_subscription_payload msg = 
  match msg.body with  
  | Subscription s -> Some s
  | _ -> None 

