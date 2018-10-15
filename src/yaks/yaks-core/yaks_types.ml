open Apero

module Property = KeyValueF.Make (String) (String) [@@deriving show]

module Str = Re.Str

module EventStream  = struct 
  include  EventStream
end 


module SubscriberId = Id.Make (Int64)
module PluginId = Id.Make (Int64)

(* This should become parametrized through a functor *)
module KeyValue =  KeyValueF.Make (String) (String) [@@deriving show]

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
  | `InternalError of error_info
] [@@deriving show]

exception YException of yerror [@@deriving show]

let () = Printexc.register_printer @@ function | YException(e) -> Some ("YException: "^(show_yerror e)) | _ -> None


open Str


let remove_useless_slashes s =
  if String.length s <= 2 then s
  else
    let buf = Buffer.create (String.length s) in
    let rec filter i =
      if i < String.length s then
        let c = String.get s i in
        if c <> '/' || (i+1 < String.length s && (String.get s (i+1) <> '/')) then
          Buffer.add_char buf c;
        filter (i+1)
    in
    let _ =
      (* Note: add 1st char anyway to preserve the starting // *)
      Buffer.add_char buf (String.get s 0);
      filter 1
    in
    Buffer.contents buf


module Path = struct
  type t = string
  (* from https://tools.ietf.org/html/rfc3986#appendix-B *)
  (* let prefix = Str.regexp "\\(/[0-9A-za-z._-]*\\)+" *)
  let path_regex = Str.regexp "[^?#*]+"

  let is_valid s = Str.string_match path_regex s 0

  let of_string_opt ?(is_absolute=true) s =
    if Astring.length s > 1 && is_valid s then
      if is_absolute && not (Astring.is_prefix ~affix:"//" s) then None
      else Some (remove_useless_slashes s)
    else None

  let of_string ?(is_absolute=true) s =
    Apero.Option.get_or_else (of_string_opt ~is_absolute s)
    (fun () -> raise (YException (`InvalidPath (`Msg s))))

  let to_string s = s

  let is_prefix ~affix path = Astring.is_prefix ~affix:(to_string affix) (to_string path)

  let compare = String.compare

  (* let matches _ _ = true *)
end [@@deriving show]

module Selector = struct

  type t = { path: string; query: string option; fragment: string option }


  (* from https://tools.ietf.org/html/rfc3986#appendix-B *)
  let sel_regex = Str.regexp "\\([^?#]*\\)\\(\\?\\([^#]*\\)\\)?\\(#\\(.*\\)\\)?$"

  let is_valid s = Str.string_match sel_regex s 0

  (* let key s = s.key *)

  let of_string_opt ?(is_absolute=true) s =
    if Astring.length s > 1 && is_valid s then
      if is_absolute && not (Astring.is_prefix ~affix:"//" s) then None
      else 
        let path = remove_useless_slashes @@ Str.matched_group 1 s
        and query = try Some(Str.matched_group 3 s) with Not_found -> None
        and fragment = try Some(Str.matched_group 5 s) with Not_found -> None
        in
        Some { path; query; fragment }
    else None

  let of_string ?(is_absolute=true) s =
    Apero.Option.get_or_else (of_string_opt ~is_absolute s)
    (fun () -> raise (YException (`InvalidPath (`Msg s))))


  let to_string s =
    Printf.sprintf "%s%s%s" s.path 
      (match s.query with | Some(q) -> "?"^q | None -> "")
      (match s.fragment with | Some(f) -> "#"^f | None -> "")

  let path s = s.path

  let query s = s.query

  let fragment s = s.fragment

  let wildcard_regex = regexp "\\(\\*\\*\\)\\|[*]"



  let string_of_sel sel =
    List.fold_left (fun acc e -> match e with | Text(t) -> acc^"::Text("^t^")" | Delim(d) -> acc^"::Delim("^d^")") "" sel

  let is_unique_path sel = not @@ Astring.contains '*' sel.path

  let as_unique_path sel = if is_unique_path sel then Some (Path.of_string sel.path) else None

  let remove_last_slash = Astring.drop ~rev:true ~sat:(fun c -> c = '/')


  let is_matching_path _ _ =    true
  
  let is_prefixing_path _ _ = true


  let is_matching ?(prefix_matching=false) path selector =
    let sel_path = remove_last_slash selector.path and path = remove_last_slash path in
    let _ = Logs_lwt.debug (fun m -> m "---- is_matching %b %s %s " prefix_matching path sel_path) in
    let rec check_matching sel path =
      let _ = Logs_lwt.debug (fun m -> m "   - check_match %s %s " (string_of_sel sel) path) in
      match (sel, path) with
      | ([], "") -> let _ = Logs_lwt.debug (fun m -> m "   - exact match => TRUE") in true
      | (_, "") -> let _ = Logs_lwt.debug (fun m -> m "   - path too short => prefix_matching=%b" prefix_matching) in prefix_matching  (* path too short: OK if prefix_matching expected, not-OK if full match expected *)
      | ([], _) -> let _ = Logs_lwt.debug (fun m -> m "   - path too long => FALSE") in false                (* path too long *)
      | (Text(t)::sel, path) ->
        if prefix_matching then
          if (Astring.is_prefix ~affix:path t) then
            let _ = Logs_lwt.debug (fun m -> m "   - path is a prefix of Text => TRUE") in true
          else if (Astring.is_prefix ~affix:t path) then
            check_matching sel (Astring.with_range ~first:(String.length t) ~len:(String.length path - String.length t) path)
          else
            let _ = Logs_lwt.debug (fun m -> m "   - path doesn't match Text => FALSE") in false
        else if (Astring.is_prefix ~affix:t path) then
          check_matching sel (Astring.with_range ~first:(String.length t) path)
        else
          let _ = Logs_lwt.debug (fun m -> m "   - path doesn't start with 1st element of selector => FALSE") in false
      | (Delim("*")::[], path) ->
        if prefix_matching then
          let _ = Logs_lwt.debug (fun m -> m "   - * at end; path is a prefix of * => TRUE") in true
        else
          if (String.contains path '/') then
            let _ = Logs_lwt.debug (fun m -> m "   - * at end but / remaining => FALSE") in false
          else
            let _ = Logs_lwt.debug (fun m -> m "   - * at end no / remaining => TRUE") in true
      | (Delim("*")::Text(t)::sel, path) ->
        let search_limit = match String.index_opt path '/' with | Some(i) -> i | None -> String.length path - 1 in
        (try
           let i = search_forward (regexp_string t) path 0 in
           if (i > search_limit) then
             let _ = Logs_lwt.debug (fun m -> m "   - text '%s' found in path but after / => FALSE" t) in false
           else
             check_matching sel (String.sub path (i+String.length t) (String.length path - i - String.length t))
         with
           Not_found -> let _ = Logs_lwt.debug (fun m -> m "   - text '%s' not found in path => FALSE" t) in false)
      | (Delim("**")::[], _) -> let _ = Logs_lwt.debug (fun m -> m "   - ** at end match all! => TRUE") in true
      | (Delim("**")::Text(t)::sel, key) ->
        (try 
           let i = search_backward (regexp_string t) key (String.length key) in
           check_matching sel (String.sub key (i+String.length t) (String.length key - i - String.length t))
         with
           Not_found -> let _ = Logs_lwt.debug (fun m -> m "   - text '%s' not found in path => FALSE" t) in false
        )
      | (Delim(_)::_, _) -> raise (Invalid_argument "Invalid Selector") (* Shouldn't happen !!!*)
    in
    check_matching (full_split wildcard_regex sel_path) path


    let remove_prefix prefix selector =
      (* NOTE: this algo assumes that matching of prefix with the selector have been checked *)
      let sel = to_string selector in
      let open Apero in
      let rec next_char ip is =
        if is >= String.length sel then ""
        else if ip >= String.length prefix then Astring.after is sel
        else match String.get sel is with
          | c when c = String.get prefix ip -> next_char (ip+1) (is+1)
          | '*' ->
            if is+1 = String.length sel then ""
            else let next_sel_char = String.get sel (is+1) in 
              if next_sel_char = '*' then   (* found '**' in selector *)
                raise (Invalid_argument "Selector with ** not supported yet in remove_prefix operation")
              else  (* found '*' in selector *)
                let ip' = String.index_from prefix (ip+1) next_sel_char in
                next_char ip' (is+1)

          | _ -> Astring.after is sel
      in
      next_char 0 0 |> of_string ~is_absolute:false

end

module Value = struct 
  type encoding = 
    | Raw_Encoding
    | String_Encoding 
    | Json_Encoding  
    | Sql_Encoding  

  type sql_row = string list
  type sql_column_names = string list

  type t  = 
    | RawValue of Lwt_bytes.t 
    | StringValue of string
    | JSonValue of string
    | SqlValue of (sql_row * sql_column_names option)

  let update _ _ = Apero.Result.fail `UnsupportedOperation

  let encoding = function 
    | RawValue _ -> Raw_Encoding
    | StringValue _ -> String_Encoding
    | JSonValue _ -> Json_Encoding
    | SqlValue _ -> Sql_Encoding

  let encoding_to_string = function 
    | Raw_Encoding -> "RAW"
    | String_Encoding -> "STRING"
    | Json_Encoding -> "JSON"
    | Sql_Encoding -> "SQL"

  let encoding_of_string s =
    if s = "STRING" then String_Encoding
    else if s = "JSON" then Json_Encoding
    else if s = "SQL" then Sql_Encoding
    else Raw_Encoding

  let sql_val_sep = Char.chr 31 (* US - unit separator *)
  let sql_val_sep_str = String.make 1 sql_val_sep
  let sql_row_sep = Char.chr 30 (* RS - record separator *)
  let sql_row_sep_str = String.make 1 sql_row_sep
  

  let sql_to_string = function
    | (row, None) -> String.concat sql_val_sep_str row
    | (row, Some col) -> (String.concat sql_val_sep_str row)^sql_row_sep_str^(String.concat sql_val_sep_str col)

  let sql_of_string s = 
    match String.split_on_char sql_row_sep s with
    | row::[] -> String.split_on_char sql_val_sep row , None
    | row::col::[] -> String.split_on_char sql_val_sep row , Some (String.split_on_char sql_val_sep col)
    | _ -> raise @@ YException (`UnsupportedTranscoding (`Msg ("String to SQL of  "^s)))

  let to_raw_encoding = function
    | RawValue _ as v -> Apero.Result.ok @@ v
    | StringValue s -> Apero.Result.ok @@ RawValue (Lwt_bytes.of_string s)
    | JSonValue s -> Apero.Result.ok @@ RawValue (Lwt_bytes.of_string s)
    | SqlValue v  -> Apero.Result.ok @@ RawValue (Lwt_bytes.of_string @@ sql_to_string v)

  let to_string_encoding = function 
    | RawValue r  -> Apero.Result.ok @@ StringValue (Lwt_bytes.to_string r)
    | StringValue _ as v  -> Apero.Result.ok @@ v
    | JSonValue s -> Apero.Result.ok @@ StringValue s
    | SqlValue v -> Apero.Result.ok @@ StringValue (sql_to_string v)

  let json_from_sql (row, col) =
    let open Yojson.Basic in
    let kv_list = match col with
    | None -> List.mapi (fun i v -> "'col_"^(string_of_int i) , `String v ) row
    | Some col -> List.map2 (fun k v -> k , `String v) col row
    in
    to_string (`Assoc kv_list)

  let to_json_encoding = 
    let open Yojson.Basic in
    function
    | RawValue r  -> Apero.Result.ok @@ JSonValue (to_string @@ `String (Lwt_bytes.to_string r))  (* @TODO: base-64 encoding? *)
    | StringValue s  -> Apero.Result.ok @@ JSonValue (to_string @@ `String s)
    | JSonValue _ as v -> Apero.Result.ok @@ v
    | SqlValue v -> Apero.Result.ok @@ StringValue (json_from_sql v)

  (* @TODO: use Error instead of Exception *)
  let sql_from_json json =
    let open Yojson.Basic in
    match from_string json with
    | `Assoc l -> List.split l |> fun (col, row) -> (List.map (fun json -> to_string json) row), Some col
    | _ -> raise @@ YException (`UnsupportedTranscoding (`Msg ("Json to SQL of  "^json)))

  let to_sql_encoding = function
    | RawValue r -> Apero.Result.ok @@ SqlValue (sql_of_string (Lwt_bytes.to_string r))
    | StringValue s  -> Apero.Result.ok @@ SqlValue (sql_of_string s)
    | JSonValue s -> Apero.Result.ok @@ SqlValue (sql_from_json s)
    | SqlValue _ as v -> Apero.Result.ok @@ v


  let transcode v = function   
    | Raw_Encoding -> to_raw_encoding v
    | String_Encoding -> to_string_encoding v
    | Json_Encoding -> to_json_encoding v
    | Sql_Encoding -> to_sql_encoding v

  let of_string s e = transcode (StringValue s)  e
  let to_string  = function 
    | RawValue r -> Lwt_bytes.to_string r
    | StringValue s -> s 
    | JSonValue j -> j 
    | SqlValue s -> sql_to_string s

end
