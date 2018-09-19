module Key = struct 
  let key = "is.yaks.backend.sql"    
  let url = "is.yaks.backend.sql.url"
  let table = "is.yaks.backend.sql.table"
  let key_size = "is.yaks.backend.sql.key_size"   (* int value *)
  let on_dispose = "is.yaks.backend.sql.on_dispose"  (* possible values: drop, truncate or not defined (i.e. do nothing) *)
end

type on_dispose = Drop | Truncate | DoNothing

let get_on_dispose props =
  match Yaks_property.get_property Key.on_dispose props with
  | Some text ->
    let t = String.uppercase_ascii text in
    if t = "DROP" then Drop
    else if t = "TRUNCATE" then Truncate
    else if t = "TRUNC" then Truncate
    else let _ = Logs_lwt.err (fun m -> m "[SQL]: unsuppoerted property: %s=%s - ignore it" Key.on_dispose text) in DoNothing
  | None -> DoNothing
