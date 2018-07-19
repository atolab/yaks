open Selector
open Ypath

let ascii_table = List.init 256 (fun i -> char_of_int i)

let acceptable_chars = List.filter (fun c -> c > ' ' && c != '/' && c != '*' && c != '?' && c != '#') ascii_table


let check_if b ?arg line =
  let test_name =
    match arg with
    | None -> Printf.sprintf "test line %d" line
    | Some(s) -> Printf.sprintf "test line %d with %s" line s
  in
  Alcotest.(check bool) test_name b

let test_selector_is_matching s p =
  Selector.is_matching (Selector.of_string s) (Ypath.of_string p)

let test_selector s path query fragment =
  try
    let sel = Selector.of_string s in
    Selector.path sel = Ypath.of_string path &&
    Selector.query sel = query &&
    Selector.fragment sel = fragment
  with 
    Invalid_argument(_) -> false

let test_validity () =
  check_if true __LINE__ @@ test_selector "/a/b/c" "/a/b/c" None None;
  check_if true __LINE__ @@ test_selector "/*" "/*" None None;
  check_if true __LINE__ @@ test_selector "/a*" "/a*" None None;
  check_if true __LINE__ @@ test_selector "/**" "/**" None None;
  check_if true __LINE__ @@ test_selector "/a**" "/a**" None None;
  check_if true __LINE__ @@ test_selector "/a/b?q" "/a/b" (Some "q") None;
  check_if true __LINE__ @@ test_selector "/a/b#f" "/a/b" None (Some "f");
  check_if true __LINE__ @@ test_selector "/a/b?q#f" "/a/b" (Some "q") (Some "f");
  ()

let test_simple_selectors () =
  check_if true __LINE__ @@ test_selector_is_matching "/" "/";
  check_if true __LINE__ @@ test_selector_is_matching "//" "//";
  check_if true __LINE__ @@ test_selector_is_matching "/a" "/a";
  check_if true __LINE__ @@ test_selector_is_matching "/a/" "/a";
  check_if true __LINE__ @@ test_selector_is_matching "/a" "/a/";
  check_if true __LINE__ @@ test_selector_is_matching "/a/b" "/a/b";
  check_if true __LINE__ @@ test_selector_is_matching "/a/b?q" "/a/b";
  check_if true __LINE__ @@ test_selector_is_matching "/a/b#f" "/a/b";
  check_if true __LINE__ @@ test_selector_is_matching "/a/b?q#f" "/a/b";
  List.iter (fun c -> let arg = Printf.sprintf "/%c" c in check_if true __LINE__ ~arg @@ test_selector_is_matching arg arg) acceptable_chars;
  ()

let test_wildcard_selectors () =
  check_if false __LINE__ @@ test_selector_is_matching "*" "";
  check_if true __LINE__ @@ test_selector_is_matching "/*" "/abc";
  check_if true __LINE__ @@ test_selector_is_matching "/*" "/abc/";
  check_if true __LINE__ @@ test_selector_is_matching "/*/" "/abc";
  check_if false __LINE__ @@ test_selector_is_matching "/*/" "//";
  check_if false __LINE__ @@ test_selector_is_matching "/*" "xxx";
  check_if true __LINE__ @@ test_selector_is_matching "/ab*" "/abcd";
  check_if false __LINE__ @@ test_selector_is_matching "/ab*" "/ab";
  check_if true __LINE__ @@ test_selector_is_matching "/a/*/c/*/e" "/a/b/c/d/e";
  check_if false __LINE__ @@ test_selector_is_matching "/a/*/c/*/e" "/a/c/e";
  check_if false __LINE__ @@ test_selector_is_matching "/a/*/c/*/e" "/a/b/c/d/x/e";
  check_if true __LINE__ @@ test_selector_is_matching "/a/*/c/*/e?q" "/a/b/c/d/e";
  check_if true __LINE__ @@ test_selector_is_matching "/a/*/c/*/e#f" "/a/b/c/d/e";
  check_if true __LINE__ @@ test_selector_is_matching "/a/*/c/*/e?q#f" "/a/b/c/d/e";
  List.iter (fun c -> let arg = Printf.sprintf "/%c" c in check_if true __LINE__ ~arg @@ test_selector_is_matching "/*" arg) acceptable_chars;
  ()

let test_2_wildcard_selectors () =
  check_if false __LINE__ @@ test_selector_is_matching "**" "";
  check_if true __LINE__ @@ test_selector_is_matching "/**" "/a/b/c";
  check_if true __LINE__ @@ test_selector_is_matching "/**" "/a/b/c/";
  check_if true __LINE__ @@ test_selector_is_matching "/**/" "/a/b/c";
  check_if false __LINE__ @@ test_selector_is_matching "/**" "xxx";
  check_if false __LINE__ @@ test_selector_is_matching "/**/" "//";
  check_if true __LINE__ @@ test_selector_is_matching "/ab**" "/abcd/ef";
  check_if false __LINE__ @@ test_selector_is_matching "/ab**" "/ab";
  check_if true __LINE__ @@ test_selector_is_matching "/a/**/c/**/e" "/a/b/b/b/c/d/d/d/e";
  check_if false __LINE__ @@ test_selector_is_matching "/a/**/c/**/e" "/a/c/e";
  check_if true __LINE__ @@ test_selector_is_matching "/a/**/c/**/e?q" "/a/b/b/b/c/d/d/d/e";
  check_if true __LINE__ @@ test_selector_is_matching "/a/**/c/**/e#f" "/a/b/b/b/c/d/d/d/e";
  check_if true __LINE__ @@ test_selector_is_matching "/a/**/c/**/e?q#f" "/a/b/b/b/c/d/d/d/e";
  List.iter (fun c -> let arg = Printf.sprintf "/%c" c in check_if true __LINE__ ~arg @@ test_selector_is_matching "/**" arg) acceptable_chars;
  ()

let all_tests = [
  "Selectors validity", `Quick, test_validity;
  "Selectors no *" , `Quick, test_simple_selectors;
  "Selectors with *" , `Quick, test_wildcard_selectors;
  "Selectors with **" , `Quick, test_2_wildcard_selectors;
]
