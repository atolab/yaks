open Yaks_api
open Lwt.Infix
let usage () = ignore( print_endline "USAGE:\n\t simple <yaks_address> <yaks_port>\n" )


let main argv = 
  let addr = Array.get argv 1 in
  let port = Array.get argv 2 in 
  let locator = Apero.Option.get @@ Apero_net.Locator.of_string @@ Printf.sprintf "tcp/%s:%s" addr port in
  let%lwt api = YApi.connect locator in
  let comp = 
    YApi.create_access (Yaks_core.Path.of_string "//afos/0") api
    >>= fun access -> YAccess.put 
      (Yaks_core.Selector.of_string "//afos/0/1")
      (Apero.Result.get (Yaks_core.Value.of_string "hello!" Yaks_core.Value.Raw_Encoding)) access
    >>= fun _ -> YAccess.get (Yaks_core.Selector.of_string "//afos/0/*") access
    >>= fun data -> List.iter (
      fun (k,v) -> 
        ignore @@ Logs_lwt.info (fun m -> m ">>>>>>>>>>>>>>> [APP] K %s - V: %s"  (Yaks_core.Selector.to_string k) (Yaks_core.Value.to_string v))
    ) data; Lwt.return_unit
    >>= fun _ -> YApi.dispose_access access api
    >>= fun _ -> YApi.close api
    (* >>= fun access -> 
       let%lwt aid = YAccess.get_id access in
       let _ = Logs_lwt.info (fun m -> m ">>>>>>>>>>>>>>> [APP] AccessID:%s " @@ YAccess.Id.to_string aid ) in
       ignore @@ 
       YAccess.put 
       (Apero.Option.get (Yaks_core.Selector.of_string "//afos/0/1"))
       (Apero.Result.get (Yaks_core.Value.of_string "hello!" Yaks_core.Value.Raw_Encoding));
       Lwt.return access
       >>= fun ac -> 
       Lwt.return (YAccess.get (Apero.Option.get (Yaks_core.Selector.of_string "//afos/0/1")) ac, ac) >>=
       fun (_,a) -> Lwt.return @@ YApi.dispose_access a >>= fun _ -> *)
  in Lwt.join [comp]


let _ =
  let argv = Sys.argv in
  let level = Apero.Result.get @@ Logs.level_of_string "debug" in
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  if Array.length argv < 3 then usage ()
  else
    begin
      Lwt_main.run (main argv)
    end



