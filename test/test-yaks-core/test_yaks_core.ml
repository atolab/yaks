
(* Run it *)
let () =
  Printexc.record_backtrace true;
  Alcotest.run "Yaks_core Test" [
    "test_selector", Test_selector.all_tests;
  ]
