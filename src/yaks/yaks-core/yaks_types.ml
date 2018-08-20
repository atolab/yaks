module Property = Apero.KeyValueF.Make (String) (String) [@@deriving show]

module Str = Re.Str

module EventStream  = struct 
  include  Apero.EventStream
end 

(* module AccessId = struct 
   include Apero.Uuid
   end [@@deriving show] *)

module StorageId = struct 
  include String
  let make () = Apero.Uuid.next_id () |> Apero.Uuid.to_string
  let of_string ?pos s = 
    match pos with
    | Some(p) -> Some(String.sub s p (String.length s - p))
    | None -> Some(s)
  let to_string ?upper s = match upper with | Some(true) -> String.uppercase_ascii s | _ -> s
  let to_bytes = to_string ~upper:false
  let of_bytes = of_string

end [@@deriving show]


module SubscriberId = Apero.Id.Make (Int64)
module PluginId = Apero.Id.Make (Int64)

(* This should become parametrized through a functor *)
module KeyValue =  Apero.KeyValueF.Make (String) (String) [@@deriving show]

type error_info = [`NoMsg | `Msg of string | `Code of int | `Pos of (string * int * int * int) | `Loc of string] [@@deriving show]  

type yerror = [
  | `InsufficientStorage
  | `Forbidden of error_info
  | `InvalidParameters
  | `UnknownStorage of error_info
  | `UnknownAccess of error_info
  | `UnknownStorageKind 
  | `UnavailableStorageFactory of error_info
  | `UnkownAccessId of error_info
  | `StoreError of error_info
  | `UnauthorizedAccess of error_info
  | `UnsupportedTranscoding of error_info
  | `UnsupportedOperation
] [@@deriving show]

exception YException of yerror [@@deriving show]


open Str

module Path = struct
  type t = string
  (* from https://tools.ietf.org/html/rfc3986#appendix-B *)
  let prefix = Str.regexp "\\(/[0-9A-za-z._-]*\\)+"
  let path_regex = Str.regexp "[^?#]*"

  let is_valid s = Str.string_match path_regex s 0

  let is_key p = 
    match Str.string_match prefix p 0 with
    | true -> (Str.matched_string p) = p 
    | false -> false

  let key p = 
    let _ = Logs_lwt.debug (fun m -> m "Key for path: -%s-" p) in
    match Str.string_match prefix p 0 with
    | true -> 
      let ms = Str.matched_string p in 
      let _ = Logs_lwt.debug (fun m -> m "Matched Key = %s" ms) in
      if ms = p then (Some p) else None
    | false -> None

  let prefix p = 
    match Str.string_match prefix p 0 with
    | true -> Str.matched_string p
    | false -> ""

  let of_string s = 
    if is_valid s then Some s else None

  let to_string s = s

  let is_prefix prefix path = Apero.String.starts_with (to_string prefix) (to_string path)

  let matches _ _ = true
end [@@deriving show]

module Selector = struct

  type t = { path: string; key: string option; query: string option; fragment: string option }


  (* from https://tools.ietf.org/html/rfc3986#appendix-B *)
  let sel_regex = Str.regexp "\\([^?#]*\\)\\(\\?\\([^#]*\\)\\)?\\(#\\(.*\\)\\)?$"

  let is_valid s = Str.string_match sel_regex s 0

  let key s = s.key

  let of_string s = 
    match is_valid s with
    | true ->
      let path = Str.matched_group 1 s
      and query = try Some(Str.matched_group 3 s) with Not_found -> None
      and fragment = try Some(Str.matched_group 5 s) with Not_found -> None
      in
      let key = 
        let open Apero.Option.Infix in 
        Path.of_string path >>= fun p -> Path.key p in
      Some { path; key; query; fragment }
    | false -> None


  let to_string s =
    Printf.sprintf "%s%s%s" s.path 
      (match s.query with | Some(q) -> "?"^q | None -> "")
      (match s.fragment with | Some(f) -> "#"^f | None -> "")

  let path s = Apero.Option.get @@ Path.of_string s.path

  let query s = s.query

  let fragment s = s.fragment

  let wildcard_regex = regexp "\\(\\*\\*\\)\\|[*]"


  let string_remove_prefix s prefix =
    let prefix_len = String.length prefix in
    let string_len = String.length s in
    String.sub s prefix_len (string_len-prefix_len)


  let string_of_sel sel =
    List.fold_left (fun acc e -> match e with | Text(t) -> acc^"::Text("^t^")" | Delim(d) -> acc^"::Delim("^d^")") "" sel


  let rec remove_last_slash key =
    if String.length key == 0 || String.get key (String.length key-1) != '/' then
      key
    else 
      remove_last_slash @@ String.sub key 0 (String.length key-1)


  let match_path selector path =
    let sel_path = remove_last_slash selector.path and key = remove_last_slash @@ Path.to_string path in
    let sel = full_split wildcard_regex sel_path in
    let rec check_matching sel key =
      Printf.printf "   - %s vs. '%s'\n" (string_of_sel sel) key;
      match (sel, key) with
      | ([], "") -> true
      | (_::[], "") -> false  (* key too short *)
      | ([], _) -> false      (* key too long *)
      | (Text(t)::sel, key) ->
        if (Apero.String.starts_with t key) then
          check_matching sel (string_remove_prefix key t)
        else
          false
      | (Delim("*")::[], key) ->
        if (String.contains key '/') then false else true
      | (Delim("*")::Text(t)::sel, key) ->
        let search_limit = match String.index_opt key '/' with | Some(i) -> i | None -> String.length key - 1 in
        (try
           let i = search_forward (regexp_string t) key 0 in
           if (i > search_limit) then
             false
           else
             check_matching sel (String.sub key (i+String.length t) (String.length key - i - String.length t))
         with
           Not_found -> false)
      | (Delim("**")::[], _) -> true
      | (Delim("**")::Text(t)::sel, key) ->
        (try 
           let i = search_backward (regexp_string t) key (String.length key) in
           check_matching sel (String.sub key (i+String.length t) (String.length key - i - String.length t))
         with
           Not_found -> false
        )
      | (Delim(_)::_, _) -> raise (Invalid_argument "Invalid Selector") (* Shouldn't happen !!!*)
    in
    check_matching sel key

  let match_string sel str = 
    match Path.of_string str with 
    | Some p -> match_path sel p
    | None -> false

  let of_string_list selectors = 
    List.map (fun e -> match of_string e with | Some s -> s | _ -> failwith "Selector.of_string_list error non valid selector after check ??") @@ List.filter (fun e -> is_valid e) selectors
  let to_string_list selectors = 
    List.map (fun e -> to_string e) selectors
end

module Value = struct 
  type encoding = 
    | Raw_Encoding
    | String_Encoding 
    | Json_Encoding  

  type t  = 
    | RawValue of Lwt_bytes.t 
    | StringValue of string
    | JSonValue of string

  let make buf = function 
    | Raw_Encoding -> RawValue buf
    | String_Encoding -> StringValue (Lwt_bytes.to_string buf)
    | Json_Encoding -> JSonValue (Lwt_bytes.to_string buf)

  let update _ _ = Apero.Result.fail `UnsupportedOperation

  let encoding = function 
    | RawValue _ -> Raw_Encoding
    | StringValue _ -> String_Encoding
    | JSonValue _ -> Json_Encoding

  let to_raw_encoding = function
    | RawValue _ as v -> Apero.Result.ok @@ v
    | StringValue s -> Apero.Result.ok @@ RawValue  (Lwt_bytes.of_string s)
    | JSonValue s -> Apero.Result.ok @@ RawValue (Lwt_bytes.of_string s)

  let to_string_encoding = function 
    | RawValue r  -> Apero.Result.ok @@ StringValue (Lwt_bytes.to_string r)
    | StringValue _ as v  -> Apero.Result.ok @@ v
    | JSonValue s -> Apero.Result.ok @@ StringValue s 

  (* @TODO: Should really do the JSON validation *)
  let to_json_encoding = function 
    | RawValue r  -> Apero.Result.ok @@ JSonValue (Lwt_bytes.to_string r)
    | StringValue s  -> Apero.Result.ok @@ JSonValue s
    | JSonValue _ as v -> Apero.Result.ok @@ v

  let transcode v = function   
    | Raw_Encoding -> to_raw_encoding v
    | String_Encoding -> to_string_encoding v
    | Json_Encoding -> to_json_encoding v

  let of_string s e = transcode (StringValue s)  e
  let to_string  = function 
    | RawValue r -> Lwt_bytes.to_string r
    | StringValue s -> s 
    | JSonValue j -> j 

  let to_bytes  = function 
    | RawValue r ->  r
    | StringValue s -> Lwt_bytes.of_string s 
    | JSonValue j -> Lwt_bytes.of_string j 

  let of_bytes = make 
end
