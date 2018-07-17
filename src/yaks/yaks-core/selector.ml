open Str

type t = string

exception Invalid_selector of string

let wildcard_regex = regexp "\\(\\*\\*\\)\\|[*]"


let string_starts_with s prefix =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len &&
  String.equal (String.sub s 0 prefix_len) prefix

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



let is_matching selector key =
  let selector = remove_last_slash selector and key = remove_last_slash key in
  let sel = full_split wildcard_regex selector in
  let rec check_matching sel key =
    match (sel, key) with
    | ([], "") -> true
    | (_::[], "") -> false  (* key too short *)
    | ([], _) -> false      (* key too long *)
    | (Text(t)::sel, key) ->
      if (string_starts_with key t) then
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
    | (Delim(_)::_, _) -> raise (Invalid_selector "Invalid Selector") (* Shouldn't happen !!!*)
  in
  check_matching sel key



