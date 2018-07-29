module EventStream = Apero.EventStream

type property = {key : string; value : string}


type plugin_kind = 
  | Backend 
  | Frontend
  | Transport
  


type entity = 
  | Access of { path : string; cache_size : int64 }
  | Storage of { path : string; properties: property list }
  | Subscriber of { access_id: string; selector : string; push : bool}
  | Plugin of { mailbox: event Actor.Actor.actor_mailbox; kind: plugin_kind }
(* FEBE -> plguin
mailbox -? addr
 *)
and entity_identifier = 
  | Yaks
  | AccessId of string 
  | StorageId of string 
  | SubscriberId of Int64.t
  | PluginId of Int64.t
  | Auto


and encoding = [
  | `String 
  | `Json
  | `XML
]

and tuple = { key: string; value: Lwt_bytes.t }

and message = 
  | Create of { cid: int64; entity : entity; entity_id : entity_identifier }
  | Dispose of { cid: int64; entity_id : entity_identifier }
  | Get of { cid: int64; entity_id : entity_identifier; key : string; encoding: encoding option }
  | Put of { cid: int64; access_id : entity_identifier; key : string; value : string }
  | Patch of { cid: int64; access_id : entity_identifier; key : string; value : string }
  | Remove of { cid: int64; access_id : entity_identifier; key : string }
  | Notify of { cid: int64; sid : entity_identifier; values: tuple list }
  | Values of { cid: int64; encoding: encoding; values : tuple list }
  | Error of { cid : int64; reason : int }
  | Ok of { cid : int64;  entity_id: entity_identifier}
  | Subscribe of { cid: int64; entity_id : entity_identifier; key : string }
  | Unsubscribe of {cid: int64; entity_id: entity_identifier}
  | AddPlugin of {entity : entity;  entity_id: entity_identifier}
  | RemovePlugin of {entity_id: entity_identifier; kind: plugin_kind}


and message_handler = message -> unit Lwt.t

and message_sink = message EventStream.Sink.s
and message_source = message EventStream.Source.s
and message_resolver = message Lwt.u

and event = 
  | EventWithHandler of message * message_handler
  | Event of message

type event_stream = event EventStream.t
type event_sink = event EventStream.Sink.s
type event_source = event EventStream.Source.s


(**********************************)
(*      helpers functions         *)
(**********************************)

let string_of_property : property -> string =
  fun p -> Printf.sprintf "%s=%s" p.key p.value

let string_of_properties p =
  String.concat ""
    [ "["; List.map string_of_property p |> String.concat ","; "]"]

let string_of_entity e = match e with
  | Access{path; cache_size} -> Printf.sprintf "Acc(%s)" path
  | Storage{path; properties}    -> Printf.sprintf "Str(%s)" path
  | Subscriber{access_id; selector; push} -> Printf.sprintf "Sub(%s)" selector
  | Plugin {mailbox; kind} -> 
    let int_of_kind = function | Backend -> 0 | Frontend -> 1 | Transport -> 2 in 
    Printf.sprintf "Kind(%d)" @@ int_of_kind kind

let string_of_entity_id eid = match eid with
  | Yaks -> "yaks"
  | AccessId s -> s
  | StorageId s -> s
  | SubscriberId i -> Int64.to_string i
  | PluginId i -> Int64.to_string i
  | Auto -> "auto"

let string_of_plugin_kind = function
  | Backend -> "Backend"
  | Frontend -> "Frontend"
  | Transport -> "Transport"

let string_of_message msg = 
  match msg with
  | Create{cid; entity; entity_id} ->
    Printf.sprintf "#%Ld Create(%s, %s)" cid (string_of_entity entity) (string_of_entity_id entity_id)
  | Dispose{cid; entity_id} ->
    Printf.sprintf "#%Ld Dispose(%s)" cid (string_of_entity_id entity_id)
  | Get{cid; entity_id; key; encoding} ->
    Printf.sprintf "#%Ld Get(%s)" cid key
  | Put{cid; access_id; key; value} ->
    Printf.sprintf "#%Ld Put(%s, %s)" cid key value
  | Patch{cid; access_id; key; value} ->
    Printf.sprintf "#%Ld Patch(%s, %s)" cid key value
  | Remove{cid; access_id; key} ->
    Printf.sprintf "#%Ld Remove(%s)" cid key
  | Notify{cid; sid; values} ->
    Printf.sprintf "#%Ld Notify(%s)" cid (string_of_entity_id sid)
  | Values{cid; encoding; values} ->
    Printf.sprintf "#%Ld Values(...)" cid
  | Error{cid; reason} ->
    Printf.sprintf "#%Ld Error(%d)" cid reason
  | Ok{cid; entity_id} ->
    Printf.sprintf "#%Ld Ok(%s)" cid (string_of_entity_id entity_id)
  | Subscribe { cid; entity_id; key } ->
    Printf.sprintf "#%Ld Subscribe(%s, %s)" cid (string_of_entity_id entity_id) key
  | Unsubscribe {cid; entity_id} ->
    Printf.sprintf "#%Ld Unsubscribe(%s)" cid (string_of_entity_id entity_id)
  | AddPlugin {entity;  entity_id} -> 
    Printf.sprintf "# AddPlugin(%s,%s)" (string_of_entity_id entity_id) (string_of_entity entity)
  | RemovePlugin {entity_id; kind} -> 
    Printf.sprintf "# RemovePlugin(%s,%s)" (string_of_entity_id entity_id) (string_of_plugin_kind kind)

let json_string_of_values values =
  values
  |> List.map (fun {key; value} -> Printf.sprintf "\"%s\":%s" key (Lwt_bytes.to_string value))
  |> String.concat ","
  |> Printf.sprintf "{%s}"

