open Apero


[%%cenum
type message_id = 
| OPEN [@id 0x01]
| CREATE [@id 0x02]
| DELETE [@id 0x03]
| PUT [@id 0xA0]
| PATCH [@id 0xA1]
| GET [@id 0xA2]
| SUB [@id 0xB0]
| UNSUB [@id 0xB1]
| EVAL [@id 0xB2]
| OK [@id 0xD0]
| ERROR [@id 0xE0]
[@@uint8_t]]


[%%cenum
type message_flags = 
| PROPERTY [@id 0x01]
| STORAGE [@id 0x02]
| ACCESS [@id 0x04]
[@@uint8_t]]

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
}

let make_header mid (mflags: message_flags list) corr_id = 
  let flags = char_of_int @@ List.fold_left (fun a f -> a lor (message_flags_to_int f)) 0 mflags in 
  {mid; flags; corr_id}

let has_property_flag h = (int_of_char h.flags) lor 0x01 <> 0
let has_storage_flag h = (int_of_char h.flags) lor 0x02 <> 0
let has_access_flag h = (int_of_char h.flags) lor 0x04 <> 0
  
type payload = 
  | Empty
  | Path of string
  | Selector of string
  | KeyValue of string * IOBuf.t
  | KeyDeltaValue of string * IOBuf.t
  | KeyValueList of (string * IOBuf.t) list
  | Subscription of string
  | Notification of string * ((string * IOBuf.t) list)
  | ErrorInfo of Vle.t * char
  

type message = {
  header: header;
  properties : Yaks_core.Property.t list;
  body : payload (* This could be a variant*)
}

let make_message header properties body = {header; properties; body} 