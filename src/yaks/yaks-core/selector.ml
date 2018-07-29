open Str
open Ypath

module Selector = struct

  type t = { path: string; query: string option; fragment: string option }


  (* from https://tools.ietf.org/html/rfc3986#appendix-B *)
  let sel_regex = Str.regexp "\\([^?#]*\\)\\(\\?\\([^#]*\\)\\)?\\(#\\(.*\\)\\)?$"

  let is_valid s = Str.string_match sel_regex s 0


  let of_string (s:string) : t = 
    if is_valid s then
      let path = Str.matched_group 1 s
      and query = try Some(Str.matched_group 3 s) with Not_found -> None
      and fragment = try Some(Str.matched_group 5 s) with Not_found -> None
      in
      { path; query; fragment }
    else 
      raise (Invalid_argument s)

  let to_string s =
    let open Apero in 
    Printf.sprintf "%s%s%s" s.path 
      (match s.query with | Some(q) -> "?"^q | None -> "")
      (match s.fragment with | Some(f) -> "#"^f | None -> "")

  let path s = Path.of_string s.path

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


  let is_matching selector path =
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

end