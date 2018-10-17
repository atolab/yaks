open Yaks_api
open Lwt.Infix
let usage () = ignore( print_endline "USAGE:\n\t simple <yaks_address> <yaks_port>\n" )


let main argv = 
  let addr = Array.get argv 1 in
  let port = Array.get argv 2 in 
  let locator = Apero.Option.get @@ Apero_net.Locator.of_string @@ Printf.sprintf "tcp/%s:%s" addr port in
  let%lwt api = YApi.connect locator in
  ignore @@ Lwt_io.printf "<<<< [APP] Creating storage on %s\n"  "//afos/0";
  let%lwt storage = YApi.create_storage (Yaks_core.Path.of_string "//afos/0") api in
  let comp = 
    ignore @@ Lwt_io.printf "<<<< [APP] Creating access on %s\n"  "//afos/0";
    YApi.create_access (Yaks_core.Path.of_string "//afos/0") api
    >>= fun access -> 
    ignore @@ Lwt_io.printf "<<<< [APP] Put %s -> %s\n" "//afos/0" "hello!";
    YAccess.put 
      (Yaks_core.Selector.of_string "//afos/0/1")
      (Apero.Result.get (Yaks_core.Value.of_string "hello!" Yaks_core.Value.Raw_Encoding)) access
    >>= fun _ -> YAccess.get (Yaks_core.Selector.of_string "//afos/0/*") access
    >>= fun data -> List.iter (
      fun (k,v) -> 
        ignore @@ Lwt_io.printf ">>>> [APP] K %s - V: %s\n"  (Yaks_core.Path.to_string k) (Yaks_core.Value.to_string v);
    ) data; Lwt.return_unit
    >>= fun _ -> 
    ignore @@ Lwt_io.printf "<<<< [APP] Dispose access\n";
    YApi.dispose_access access api
    >>= fun _ -> 
    ignore @@ Lwt_io.printf "<<<< [APP] Dispose storage\n";
    YApi.dispose_storage storage api
    >>= fun _ -> 
    ignore @@ Lwt_io.printf "<<<< [APP] Bye!\n";
    YApi.close api
  in Lwt.join [comp]


let _ =
  let argv = Sys.argv in
  let level = Apero.Result.get @@ Logs.level_of_string "error" in
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  if Array.length argv < 3 then usage ()
  else
    begin
      Lwt_main.run (main argv)
    end



