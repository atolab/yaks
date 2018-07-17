
let ascii_table = List.init 256 (fun i -> char_of_int i)

let acceptable_chars = List.filter (fun c -> c > ' ' && c != '/' && c != '*' && c != '?') ascii_table


let check_if b ?arg line =
  let test_name =
    match arg with
    | None -> Printf.sprintf "test line %d" line
    | Some(s) -> Printf.sprintf "test line %d with %s" line s
  in
  Alcotest.(check bool) test_name b


let test_simple_selectors () =
  check_if true __LINE__ @@ Selector.is_matching "/" "/";
  check_if true __LINE__ @@ Selector.is_matching "//" "//";
  check_if true __LINE__ @@ Selector.is_matching "/a" "/a";
  check_if true __LINE__ @@ Selector.is_matching "/a/" "/a";
  check_if true __LINE__ @@ Selector.is_matching "/a" "/a/";
  check_if true __LINE__ @@ Selector.is_matching "/a/b" "/a/b";
  List.iter (fun c -> let arg = Printf.sprintf "/%c" c in check_if true __LINE__ ~arg @@ Selector.is_matching arg arg) acceptable_chars;
  ()

let test_wildcard_selectors () =
  check_if false __LINE__ @@ Selector.is_matching "*" "";
  check_if true __LINE__ @@ Selector.is_matching "/*" "/abc";
  check_if true __LINE__ @@ Selector.is_matching "/*" "/abc/";
  check_if true __LINE__ @@ Selector.is_matching "/*/" "/abc";
  check_if false __LINE__ @@ Selector.is_matching "/*/" "//";
  check_if false __LINE__ @@ Selector.is_matching "/*" "xxx";
  check_if true __LINE__ @@ Selector.is_matching "/ab*" "/abcd";
  check_if false __LINE__ @@ Selector.is_matching "/ab*" "/ab";
  check_if true __LINE__ @@ Selector.is_matching "/a/*/c/*/e" "/a/b/c/d/e";
  check_if false __LINE__ @@ Selector.is_matching "/a/*/c/*/e" "/a/c/e";
  check_if false __LINE__ @@ Selector.is_matching "/a/*/c/*/e" "/a/b/c/d/x/e";
  List.iter (fun c -> let arg = Printf.sprintf "/%c" c in check_if true __LINE__ ~arg @@ Selector.is_matching "/*" arg) acceptable_chars;
  ()

let test_2_wildcard_selectors () =
  check_if false __LINE__ @@ Selector.is_matching "**" "";
  check_if true __LINE__ @@ Selector.is_matching "/**" "/a/b/c";
  check_if true __LINE__ @@ Selector.is_matching "/**" "/a/b/c/";
  check_if true __LINE__ @@ Selector.is_matching "/**/" "/a/b/c";
  check_if false __LINE__ @@ Selector.is_matching "/**" "xxx";
  check_if false __LINE__ @@ Selector.is_matching "/**/" "//";
  check_if true __LINE__ @@ Selector.is_matching "/ab**" "/abcd/ef";
  check_if false __LINE__ @@ Selector.is_matching "/ab**" "/ab";
  check_if true __LINE__ @@ Selector.is_matching "/a/**/c/**/e" "/a/b/b/b/c/d/d/d/e";
  check_if false __LINE__ @@ Selector.is_matching "/a/**/c/**/e" "/a/c/e";
  List.iter (fun c -> let arg = Printf.sprintf "/%c" c in check_if true __LINE__ ~arg @@ Selector.is_matching "/**" arg) acceptable_chars;
  ()

let all_tests = [
  "Selectors no *" , `Quick, test_simple_selectors;
  "Selectors with *" , `Quick, test_wildcard_selectors;
  "Selectors with **" , `Quick, test_2_wildcard_selectors;
]
