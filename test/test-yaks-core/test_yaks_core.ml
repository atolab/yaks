
(* Run it *)
let () =
  Alcotest.run "Yaks_core Test" [
    "test_selector", Test_selector.all_tests;
  ]
