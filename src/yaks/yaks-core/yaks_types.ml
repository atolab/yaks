module Property = Apero.KeyValueF.Make (String) (String) [@@deriving show]

module Str = Re.Str

module EventStream  = struct 
  include  Apero.EventStream
end 


module SubscriberId = Apero.Id.Make (Int64)
module PluginId = Apero.Id.Make (Int64)

(* This should become parametrized through a functor *)
module KeyValue =  Apero.KeyValueF.Make (String) (String) [@@deriving show]

type error_info = [`NoMsg | `Msg of string | `Code of int | `Pos of (string * int * int * int) | `Loc of string] [@@deriving show]  

type yerror = [
  | `InvalidBackendProperty of error_info
  | `InsufficientStorage
  | `InvalidPath of error_info
  | `Forbidden of error_info
  | `InvalidParameters
  | `ConflictingStorage of error_info
  | `NoCompatibleBackend of error_info
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
  (* let prefix = Str.regexp "\\(/[0-9A-za-z._-]*\\)+" *)
  let path_regex = Str.regexp "//[^?#*]*"

  let is_valid s = Str.string_match path_regex s 0

  (* let is_key p = 
     match Str.string_match prefix p 0 with
     | true -> (Str.matched_string p) = p 
     | false -> false *)

  (* let key p = 
     let _ = Logs_lwt.debug (fun m -> m "Key for path: -%s-" p) in
     match Str.string_match prefix p 0 with
     | true -> 
      let ms = Str.matched_string p in 
      let _ = Logs_lwt.debug (fun m -> m "Matched Key = %s" ms) in
      if ms = p then (Some p) else None
     | false -> None *)

  (* let prefix p = 
     match Str.string_match prefix p 0 with
     | true -> Str.matched_string p
     | false -> "" *)

  let of_string s =
    if is_valid s then s else raise (YException (`InvalidPath (`Msg s)))

  let of_string_opt s = 
    if is_valid s then Some s else None

  let to_string s = s

  let is_prefix prefix path = Apero.String.starts_with (to_string path) (to_string prefix)

  let compare = String.compare

  (* let matches _ _ = true *)
end [@@deriving show]

module Selector = struct

  type t = { path: string; query: string option; fragment: string option }


  (* from https://tools.ietf.org/html/rfc3986#appendix-B *)
  let sel_regex = Str.regexp "\\([^?#]*\\)\\(\\?\\([^#]*\\)\\)?\\(#\\(.*\\)\\)?$"

  let is_valid s = Str.string_match sel_regex s 0

  (* let key s = s.key *)

  let of_string s = 
    match is_valid s with
    | true ->
      let path = Str.matched_group 1 s
      and query = try Some(Str.matched_group 3 s) with Not_found -> None
      and fragment = try Some(Str.matched_group 5 s) with Not_found -> None
      in
      Some { path; query; fragment }
    | false -> None


  let to_string s =
    Printf.sprintf "%s%s%s" s.path 
      (match s.query with | Some(q) -> "?"^q | None -> "")
      (match s.fragment with | Some(f) -> "#"^f | None -> "")

  let path s = s.path

  let query s = s.query

  let fragment s = s.fragment

  let wildcard_regex = regexp "\\(\\*\\*\\)\\|[*]"


  let string_remove_prefix s prefix =
    let prefix_len = String.length prefix in
    let string_len = String.length s in
    String.sub s prefix_len (string_len-prefix_len)


  let string_of_sel sel =
    List.fold_left (fun acc e -> match e with | Text(t) -> acc^"::Text("^t^")" | Delim(d) -> acc^"::Delim("^d^")") "" sel

  let is_unique_path sel = not @@ String.contains sel.path '*'

  let as_unique_path sel = if is_unique_path sel then Some (Path.of_string sel.path) else None

  let rec remove_last_slash key =
    if String.length key == 0 || String.get key (String.length key-1) != '/' then
      key
    else 
      String.sub key 0 (String.length key-1) |> remove_last_slash

  let is_matching ?(prefix_matching=false) path selector =
    let sel_path = remove_last_slash selector.path and path = remove_last_slash path in
    let _ = Logs_lwt.debug (fun m -> m "---- is_matching %b %s %s " prefix_matching path sel_path) in
    let sel = full_split wildcard_regex sel_path in
    let rec check_matching sel path =
      let _ = Logs_lwt.debug (fun m -> m "   - check_match %s %s " (string_of_sel sel) path) in
      match (sel, path) with
      | ([], "") -> let _ = Logs_lwt.debug (fun m -> m "   - exact match ") in true
      | (_::[], "") -> let _ = Logs_lwt.debug (fun m -> m "   - path too short ") in prefix_matching  (* path too short: OK if prefix_matching expected, not-OK if full match expected *)
      | ([], _) -> let _ = Logs_lwt.debug (fun m -> m "   - path too long ") in false                (* path too long *)
      | (Text(t)::sel, path) ->
        if prefix_matching then
          if (Apero.String.starts_with t path) then true
          else false
        else if (Apero.String.starts_with path t) then
          check_matching sel (string_remove_prefix path t)
        else
          let _ = Logs_lwt.debug (fun m -> m "   - path doesn't start with 1st element of selector ") in false
      | (Delim("*")::[], path) ->
        if (String.contains path '/') then false else true
      | (Delim("*")::Text(t)::sel, path) ->
        let search_limit = match String.index_opt path '/' with | Some(i) -> i | None -> String.length path - 1 in
        (try
           let i = search_forward (regexp_string t) path 0 in
           if (i > search_limit) then
             let _ = Logs_lwt.debug (fun m -> m "   - text '%s' found in path but after /" t) in false
           else
             check_matching sel (String.sub path (i+String.length t) (String.length path - i - String.length t))
         with
           Not_found -> let _ = Logs_lwt.debug (fun m -> m "   - text '%s' not found in path" t) in false)
      | (Delim("**")::[], _) -> let _ = Logs_lwt.debug (fun m -> m "   - ** as end match all! ") in true
      | (Delim("**")::Text(t)::sel, key) ->
        (try 
           let i = search_backward (regexp_string t) key (String.length key) in
           check_matching sel (String.sub key (i+String.length t) (String.length key - i - String.length t))
         with
           Not_found -> let _ = Logs_lwt.debug (fun m -> m "   - text '%s' not found in path" t) in false
        )
      | (Delim(_)::_, _) -> raise (Invalid_argument "Invalid Selector") (* Shouldn't happen !!!*)
    in
    check_matching sel path

  (* let match_string sel str = 
     match Path.of_string str with 
     | Some p -> match_path sel p
     | None -> false *)

  (* let of_string_list selectors = 
     List.map (fun e -> match of_string e with | Some s -> s | _ -> failwith "Selector.of_string_list error non valid selector after check ??") @@ List.filter (fun e -> is_valid e) selectors *)
  (* let to_string_list selectors = 
     List.map (fun e -> to_string e) selectors *)
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
