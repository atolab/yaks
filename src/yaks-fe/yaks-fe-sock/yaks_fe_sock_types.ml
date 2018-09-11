open Apero
open Yaks_core
open Yaks_fe_sock_codes

(* The structure of a socket front end message is the following.

      7 6 5 4 3 2 1 0
    +-+-+-+-+-+-+-+-+  ---+
    |  MESSAGE CODE |     |
    +-+-+-+-+-+-+-+-+     |
    |X|X|E|N|C|A|S|P|     +--> Header
    +-+-+-+-+-+-+-+-+     | 
    ~   Coor. ID    ~     |
    +---------------+     |
    ~   Properties  ~     +-> Present if P = 1
    +---------------+-----+
    ~     Body      ~ --> its structure depends on the message code
    +---------------+   

   For transports that do not preserve message boundaries, the framing is done by prepending
   the lenght encoded using VLE.  *)

type header = { 
  mid : message_id;
  flags : char;
  corr_id : Vle.t;  
  properties : Yaks_core.properties;
}

let make_header mid (mflags: message_flags list) corr_id properties = 
  let base_flags = List.fold_left (fun a f -> a lor (message_flags_to_int f)) 0 mflags in 
  let flags = char_of_int @@ match Property.Map.is_empty properties with 
    | true ->  base_flags
    | false -> (message_flags_to_int PROPERTY) lor base_flags
  in 
  {mid; flags; corr_id; properties}

let has_property_flag flags = (int_of_char flags) land 0x01 <> 0
let has_storage_flag flags = (int_of_char flags) land 0x02 <> 0
let has_access_flag flags = (int_of_char flags) land 0x04 <> 0
let get_encoding flags = 
  match int_to_value_encoding @@ (int_of_char flags) land 0x18 with 
  | Some e -> e 
  | None -> ENCODING_INVALID
  
type payload = 
  | YEmpty
  | YPath of Yaks_core.Path.t
  | YSelector of Yaks_core.Selector.t
  | YKeyDeltaValue of Yaks_core.Selector.t * Yaks_core.Value.t
  | YKeyValueList of (Yaks_core.Selector.t * Yaks_core.Value.t) list
  | YSubscription of string
  | YNotification of string * ((Yaks_core.Path.t * Yaks_core.Value.t) list)
  | YErrorInfo of Vle.t
  

type message = {
  header: header;  
  body : payload (* This could be a variant*)
}

let make_message header body = {header; body} 

let get_empty_payload msg = 
  match msg.body with 
  | YEmpty -> Some ()
  | _ -> None

let get_path_payload msg = 
  match msg.body with 
  | YPath p -> Some p 
  | _ -> None 

let get_selector_payload msg = 
  match msg.body with 
  | YSelector s -> Some s
  | _ -> None 

let get_key_delta_value_payload msg = 
  match msg.body with  
  | YKeyDeltaValue (k,dv) -> Some (k,dv)
  | _ -> None 

let get_key_value_list_payload msg = 
  match msg.body with  
  | YKeyValueList kvs -> Some kvs
  | _ -> None 

let get_subscription_payload msg = 
  match msg.body with  
  | YSubscription s -> Some s
  | _ -> None 

