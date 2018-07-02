module EventStream = Event_stream.EventStream.Make(Stream_lwt.Stream)

type message = Yaks_codec.Message.message
type message_handler = message -> unit Lwt.t

type message_sink = Yaks_codec.Message.message EventStream.Sink.s
type message_source = Yaks_codec.Message.message EventStream.Source.s
type message_resolver = Yaks_codec.Message.message Lwt.u

type event = 
  | EventWithHandler of Yaks_codec.Message.message * message_handler
  | Event of Yaks_codec.Message.message

type event_stream = event EventStream.t
type event_sink = event EventStream.Sink.s
type event_source = event EventStream.Source.s
