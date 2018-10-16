open Yaks_types

let ascii_table = List.init 256 (fun i -> char_of_int i)

let acceptable_chars = List.filter (fun c -> c > ' ' && c != '/' && c != '*' && c != '?' && c != '#') ascii_table


let check_if b ?arg line =
  let test_name =
    match arg with
    | None -> Printf.sprintf "test line %d" line
    | Some(s) -> Printf.sprintf "test line %d with %s" line s
  in
    Alcotest.(check bool) test_name b

let test_full_matching s p =
  try 
    Selector.is_matching_path (Path.of_string p) (Selector.of_string s) 
  with 
    | YException e -> print_endline @@ show_yerror e; false

let test_prefix_matching s p =
  try 
    Selector.is_prefixed_by_path (Path.of_string p) (Selector.of_string s) 
  with 
    | YException e -> print_endline @@ show_yerror e; false



let test_selector s path query fragment =
  try
    let sel = Selector.of_string s in
    Selector.path sel = path &&
    Selector.query sel = query &&
    Selector.fragment sel = fragment
  with 
    | YException e -> print_endline @@ show_yerror e; false

let test_validity () =
  check_if true  __LINE__ @@ test_selector "//a/b/c" "//a/b/c" None None;
  check_if true  __LINE__ @@ test_selector "//a b c" "//a b c" None None;
  check_if true  __LINE__ @@ test_selector "//a b/c" "//a b/c" None None;
  check_if true  __LINE__ @@ test_selector "//*" "//*" None None;
  check_if true  __LINE__ @@ test_selector "//a*" "//a*" None None;
  check_if true  __LINE__ @@ test_selector "//**" "//**" None None;
  check_if true  __LINE__ @@ test_selector "//a**" "//a**" None None;
  check_if true  __LINE__ @@ test_selector "//a/b?q" "//a/b" (Some "q") None;
  check_if true  __LINE__ @@ test_selector "//a/b?q1?q2" "//a/b" (Some "q1?q2") None;
  check_if true  __LINE__ @@ test_selector "//a/b#f" "//a/b" None (Some "f");
  check_if true  __LINE__ @@ test_selector "//a/b#f1#f2" "//a/b" None (Some "f1#f2");
  check_if true  __LINE__ @@ test_selector "//a/b?q#f" "//a/b" (Some "q") (Some "f");
  check_if true  __LINE__ @@ test_selector "//a/b?q1?q2#f1#f2?q3" "//a/b" (Some "q1?q2") (Some "f1#f2?q3");
  check_if true  __LINE__ @@ test_selector "//" "//" None None;
  check_if true  __LINE__ @@ test_selector "//a/" "//a" None None;
  check_if true  __LINE__ @@ test_selector "////a/" "//a" None None;
  check_if true  __LINE__ @@ test_selector "////a///" "//a" None None;
  check_if true  __LINE__ @@ test_selector "////a//b///c/" "//a/b/c" None None;
  check_if false __LINE__ @@ test_selector "" "" None None;
  check_if false __LINE__ @@ test_selector "/" "" None None;
  check_if false __LINE__ @@ test_selector "abc" "" None None;
  check_if false __LINE__ @@ test_selector "/abc" "" None None;
  ()

let test_simple_selectors () =
  check_if true __LINE__ @@ test_full_matching "//" "//";
  check_if true __LINE__ @@ test_full_matching "//a" "//a";
  check_if true __LINE__ @@ test_full_matching "//a/" "//a";
  check_if true __LINE__ @@ test_full_matching "//a" "//a/";
  check_if true __LINE__ @@ test_full_matching "//a/b" "//a/b";
  check_if true __LINE__ @@ test_full_matching "//a/b?q" "//a/b";
  check_if true __LINE__ @@ test_full_matching "//a/b#f" "//a/b";
  check_if true __LINE__ @@ test_full_matching "//a/b?q#f" "//a/b";
  List.iter (fun c -> let arg = Printf.sprintf "//%c" c in check_if true __LINE__ ~arg @@ test_full_matching arg arg) acceptable_chars;
  ()

let test_wildcard_selectors () =
  check_if true  __LINE__ @@ test_full_matching "//*" "//abc";
  check_if true  __LINE__ @@ test_full_matching "//*" "//abc/";
  check_if true  __LINE__ @@ test_full_matching "//*/" "//abc";
  check_if false __LINE__ @@ test_full_matching "//*/" "///";
  check_if false __LINE__ @@ test_full_matching "//*" "xxx";
  check_if true  __LINE__ @@ test_full_matching "//ab*" "//abcd";
  check_if true  __LINE__ @@ test_full_matching "//ab*d" "//abcd";
  check_if true  __LINE__ @@ test_full_matching "//ab*" "//ab";
  check_if false __LINE__ @@ test_full_matching "//ab/*" "//ab";
  check_if true  __LINE__ @@ test_full_matching "//a/*/c/*/e" "//a/b/c/d/e";
  check_if false __LINE__ @@ test_full_matching "//a/*/c/*/e" "//a/c/e";
  check_if false __LINE__ @@ test_full_matching "//a/*/c/*/e" "//a/b/c/d/x/e";
  check_if true  __LINE__ @@ test_full_matching "//a/*/c/*/e?q" "//a/b/c/d/e";
  check_if true  __LINE__ @@ test_full_matching "//a/*/c/*/e#f" "//a/b/c/d/e";
  check_if true  __LINE__ @@ test_full_matching "//a/*/c/*/e?q#f" "//a/b/c/d/e";
  check_if false __LINE__ @@ test_full_matching "//ab*cd" "//abxxcxxd";
  check_if true  __LINE__ @@ test_full_matching "//ab*cd" "//abxxcxxcd";
  check_if false __LINE__ @@ test_full_matching "//ab*cd" "//abxxcxxcdx";
  List.iter (fun c -> let arg = Printf.sprintf "//%c" c in check_if true __LINE__ ~arg @@ test_full_matching "//*" arg) acceptable_chars;
  ()

let test_2_wildcard_selectors () =
  check_if true  __LINE__ @@ test_full_matching "//**" "//abc";
  check_if true  __LINE__ @@ test_full_matching "//**" "//a/b/c";
  check_if true  __LINE__ @@ test_full_matching "//**" "//a/b/c/";
  check_if true  __LINE__ @@ test_full_matching "//**/" "//a/b/c";
  check_if false __LINE__ @@ test_full_matching "//**/" "///";
  check_if true  __LINE__ @@ test_full_matching "//ab**" "//abcd/ef";
  check_if true  __LINE__ @@ test_full_matching "//ab**" "//ab";
  check_if false __LINE__ @@ test_full_matching "//ab/**" "//ab";
  check_if true  __LINE__ @@ test_full_matching "//a/**/c/**/e" "//a/b/b/b/c/d/d/d/e";
  check_if false __LINE__ @@ test_full_matching "//a/**/c/**/e" "//a/c/e";
  check_if true  __LINE__ @@ test_full_matching "//a/**/c/**/e?q" "//a/b/b/b/c/d/d/d/e";
  check_if true  __LINE__ @@ test_full_matching "//a/**/c/**/e#f" "//a/b/b/b/c/d/d/d/e";
  check_if true  __LINE__ @@ test_full_matching "//a/**/c/**/e?q#f" "//a/b/b/b/c/d/d/d/e";
  check_if true  __LINE__ @@ test_full_matching "//a/**/c/*/e/*" "//a/b/b/b/c/d/d/c/d/e/f";
  check_if false __LINE__ @@ test_full_matching "//a/**/c/*/e/*" "//a/b/b/b/c/d/d/c/d/d/e/f";
  check_if false __LINE__ @@ test_full_matching "//ab**cd" "//abxxcxxc/d";
  check_if true  __LINE__ @@ test_full_matching "//ab**cd" "//abxxcxx/cd";
  check_if false __LINE__ @@ test_full_matching "//ab*cd" "//abxxcxxcdx";
  List.iter (fun c -> let arg = Printf.sprintf "//%c" c in check_if true __LINE__ ~arg @@ test_full_matching "//**" arg) acceptable_chars;
  ()

let test_prefix_match () =
  check_if true   __LINE__ @@ test_prefix_matching "//a/b/c" "//";
  check_if true   __LINE__ @@ test_prefix_matching "//a/b/c" "//a/b";
  check_if false  __LINE__ @@ test_prefix_matching "//a/b/c" "//a/x";
  check_if true   __LINE__ @@ test_prefix_matching "//*" "//";
  check_if true   __LINE__ @@ test_prefix_matching "//a/*" "//a";
  check_if true   __LINE__ @@ test_prefix_matching "//a/*" "//a/";
  check_if true   __LINE__ @@ test_prefix_matching "//a/*" "//a/xxx";
  check_if true   __LINE__ @@ test_prefix_matching "//a/*" "//a/xxx/";
  check_if false  __LINE__ @@ test_prefix_matching "//a/*" "//a/xx/yy";
  check_if true   __LINE__ @@ test_prefix_matching "//a/b/*" "//a";
  check_if true   __LINE__ @@ test_prefix_matching "//a/*cd" "//a/b";
  check_if true   __LINE__ @@ test_prefix_matching "//a/*/*/x" "//a/b";
  check_if true   __LINE__ @@ test_prefix_matching "//a/*/*/x" "//a/b/c";
  check_if true   __LINE__ @@ test_prefix_matching "//a/*/*/x" "//a/b/c/x";
  check_if false  __LINE__ @@ test_prefix_matching "//a/*/*/x" "//a/b/c/d";
  check_if true   __LINE__ @@ test_prefix_matching "//a/**" "//a/b";
  check_if true   __LINE__ @@ test_prefix_matching "//a/**/x/y" "//a/b";
  ()

let all_tests = [
  "Selectors validity", `Quick, test_validity;
  "Selectors no *" , `Quick, test_simple_selectors;
  "Selectors with *" , `Quick, test_wildcard_selectors;
  "Selectors with **" , `Quick, test_2_wildcard_selectors;
  "Prefix matching" , `Quick, test_prefix_match;
]
