open Apero

module Path = struct
  include Yaks_common_types.Path

  let is_prefix ~affix path = Astring.is_prefix ~affix:(to_string affix) (to_string path)

  let remove_prefix ~prefix path =
    Astring.after (Astring.length @@ to_string prefix) (to_string path) |> of_string ~is_absolute:false
end [@@deriving show]

module Selector = struct
  include Yaks_common_types.Selector

  let of_path p = of_string @@ Path.to_string p

  let is_unique_path sel = not @@ Astring.contains '*' (get_path sel)

  let as_unique_path sel = if is_unique_path sel then Some (Path.of_string @@ get_path sel) else None

  let remove_last_slash = Astring.drop ~rev:true ~sat:(fun c -> c = '/')

  let simple_wildcard = Astring.Sub.v "*"
  let double_wildcard = Astring.Sub.v "**"

  let non_wildcard c = c != '*'
  let is_slash c = c = '/'

  let get_prefix_before_wildcard s = Astring.Sub.take ~sat:non_wildcard s

  let get_prefix_before_wildcard_until_slash s = 
    let open Astring.Sub in 
    let prefix = take ~sat:non_wildcard s in
    match find is_slash prefix with
    | Some slash -> with_index_range ~first:(start_pos prefix) ~last:(start_pos slash) @@ base prefix
    | None -> prefix

  let get_prefix_until_slash s =
    let open Astring.Sub in 
    match find is_slash s with
    | Some slash -> with_index_range ~first:(start_pos s) ~last:(start_pos slash) @@ base s
    | None -> s

  let is_matching_path path selector =
    let open Astring.Sub in
    let sel_path = v @@ remove_last_slash (get_path selector) in
    let path = v @@ remove_last_slash (Path.to_string path) in
    let _ = Logs_lwt.debug (fun m -> m "---- is_matching_path %a %a " pp path pp sel_path) in
    let rec check_matching pat sel =
      let _ = Logs_lwt.debug (fun m -> m "   - check_match %a %a " pp pat pp sel) in
      (* if selector is empty *)
      if is_empty sel then
        if is_empty pat then let _ = Logs_lwt.debug (fun m -> m "     - selector is empty and path too => TRUE") in true
        else let _ = Logs_lwt.debug (fun m -> m "     - path is too long (remaining: %a) => FALSE" pp pat) in false
      (* if selector starts with '**' *)
      else if is_prefix ~affix:double_wildcard sel then
        if length sel <= 2 then
          let _ = Logs_lwt.debug (fun m -> m "     - ** at end match all! => TRUE") in true
        else (
          let sub = with_range ~first:2 sel |> get_prefix_before_wildcard in
          match find_sub ~rev:true ~sub pat with
          | None -> let _ = Logs_lwt.debug (fun m -> m "     - %a substring not found in %a => FALSE" pp sub pp pat) in false
          | Some sub' ->
            let _ = Logs_lwt.debug (fun m -> m "     - %a substring found at %d => go on..." pp sub (start_pos sub')) in
            let pat_tail = with_range ~first:(stop_pos sub') path in
            let sel_tail = with_range ~first:(2+(length sub)) sel in
            check_matching pat_tail sel_tail)
      (* if selector starts with '*' *)
      else if is_prefix ~affix:simple_wildcard sel then
        if length sel <= 1 then (
          if exists is_slash pat then
            let _ = Logs_lwt.debug (fun m -> m "     - * at end but remaining / in %a => FALSE" pp pat) in false
          else
            let _ = Logs_lwt.debug (fun m -> m "     - * at end and no / in %a => TRUE" pp pat) in true)
        else (
          let sub_sel = with_range ~first:1 sel |> get_prefix_before_wildcard_until_slash in
          let sub_pat = get_prefix_until_slash pat in
          match find_sub ~sub:sub_sel sub_pat with
          | None -> let _ = Logs_lwt.debug (fun m -> m "     - %a substring not found in %a => FALSE" pp sub_sel pp sub_pat) in false
          | Some sub' ->
            let _ = Logs_lwt.debug (fun m -> m "     - %a substring found at %d => go on..." pp sub_sel (start_pos sub')) in
            let pat_tail = with_range ~first:(stop_pos sub') path in
            let sel_tail = with_range ~first:(1+(length sub_sel)) sel in
            check_matching pat_tail sel_tail)
      (* selector doesn't start with wildcard *)
      else
        let sub = get_prefix_before_wildcard sel in
        if is_prefix ~affix:sub pat then
          let _ = Logs_lwt.debug (fun m -> m "     - %a substring is a prefix of %a => go on..." pp sub pp pat) in
          let first = length sub in
          check_matching (with_range ~first pat) (with_range ~first sel)
        else 
          let _ = Logs_lwt.debug (fun m -> m "     - %a substring is not a prefix of %a => FALSE" pp sub pp pat) in false
    in check_matching path sel_path

  let is_prefixed_by_path path selector =
    let open Astring.Sub in
    let sel_path = v @@ remove_last_slash (get_path selector) in
    let path = v @@ remove_last_slash (Path.to_string path) in
    let _ = Logs_lwt.debug (fun m -> m "---- is_prefixed_by_path %a %a " pp path pp sel_path) in
    let rec check_is_prefixed pat sel =
      let _ = Logs_lwt.debug (fun m -> m "   - check_is_prefixed %a %a " pp pat pp sel) in
      (* if path is empty *)
      if is_empty pat then
        let _ = Logs_lwt.debug (fun m -> m "     - path is empty, it's indeed a prefix of anything => TRUE") in true
      (* if selector is empty *)
      else if is_empty sel then
        let _ = Logs_lwt.debug (fun m -> m "     - path is too long (remaining: %a) => FALSE" pp pat) in false
      (* if selector starts with '**' *)
      else if is_prefix ~affix:double_wildcard sel then
          let _ = Logs_lwt.debug (fun m -> m "     - ** found: %a is indeed prefix of %a => TRUE" pp pat pp sel) in true
      (* if selector starts with '*' *)
      else if is_prefix ~affix:simple_wildcard sel then
        if not @@ exists is_slash pat then
          let _ = Logs_lwt.debug (fun m -> m "     - * and %a has no / it's a prefix of %a => TRUE" pp pat pp sel) in true
        else if length sel <= 1 then
          let _ = Logs_lwt.debug (fun m -> m "     - * at end but remaining / in %a => FALSE" pp pat) in false
        else (
          let sub_sel = with_range ~first:1 sel |> get_prefix_before_wildcard_until_slash in
          let sub_pat = get_prefix_until_slash pat in
          match find_sub ~sub:sub_sel sub_pat with
          | None -> let _ = Logs_lwt.debug (fun m -> m "     - %a substring not found in %a => FALSE" pp sub_sel pp sub_pat) in false
          | Some sub' ->
            let _ = Logs_lwt.debug (fun m -> m "     - %a substring found at %d => go on..." pp sub_sel (start_pos sub')) in
            let pat_tail = with_range ~first:(stop_pos sub') path in
            let sel_tail = with_range ~first:(1+(length sub_sel)) sel in
            check_is_prefixed pat_tail sel_tail)
      (* selector doesn't start with wildcard *)
      else
        let sub = get_prefix_before_wildcard sel in
        if is_prefix ~affix:pat sub then
          let _ = Logs_lwt.debug (fun m -> m "     - %a substring is a prefix of %a => TRUE" pp pat pp sub) in true
        else if is_prefix ~affix:sub pat then
          let _ = Logs_lwt.debug (fun m -> m "     - %a substring is a prefix of %a => go on..." pp sub pp pat) in
          let first = length sub in
          check_is_prefixed (with_range ~first pat) (with_range ~first sel)
        else
          let _ = Logs_lwt.debug (fun m -> m "     - %a substring is not a prefix of %a => FALSE" pp sub pp pat) in false
    in check_is_prefixed path sel_path


  let remove_prefix prefix selector =
    (* NOTE: this algo assumes that matching of prefix with the selector have been checked *)
    let prefix = Path.to_string prefix in
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

module Value = Yaks_common_types.Value

module AccessId = Yaks_common_types.AccessId
module StorageId = Yaks_common_types.StorageId
module SubscriberId = Yaks_common_types.SubscriberId
