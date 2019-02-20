module Key = struct 
  let key = "is.yaks.backend.sql"    
  let url = "is.yaks.backend.sql.url"
  let table = "is.yaks.backend.sql.table"
  let key_size = "is.yaks.backend.sql.key_size"   (* int value *)
  let on_dispose = "is.yaks.backend.sql.on_dispose"  (* possible values: drop, truncate or not defined (i.e. do nothing) *)
end
