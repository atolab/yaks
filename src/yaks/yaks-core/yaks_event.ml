module EventStream = Event_stream.EventStream.Make(Stream_lwt.Stream)

type property = {key : string; value : string}

type entity = 
  | Access of { path : string; cache_size : int64 }
  | Storage of { path : string; properties: property list }
  | Subscriber of { access_id: string; selector : string; push : bool}

type entity_identifier = 
  | Yaks
  | AccessId of string 
  | StorageId of string 
  | SubscriberId of int64
  | Auto

type encoding = [
  | `String 
  | `Json
  | `XML
]

type tuple = { key: string; value: Lwt_bytes.t }

type message = 
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



type message_handler = message -> unit Lwt.t

type message_sink = message EventStream.Sink.s
type message_source = message EventStream.Source.s
type message_resolver = message Lwt.u

type event = 
  | EventWithHandler of message * message_handler
  | Event of message

type event_stream = event EventStream.t
type event_sink = event EventStream.Sink.s
type event_source = event EventStream.Source.s