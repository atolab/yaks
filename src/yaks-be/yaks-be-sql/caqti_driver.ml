type database_type = | POSTGRESQL | MARIADB | SQLITE3

type connection = {
  pool: (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t;
  db_type: database_type
}


let fail_on_error res = match%lwt res with
  | Ok r -> Lwt.return r
  | Error e -> 
    let%lwt _ = Logs_lwt.err (fun m -> m "[SQL]: %s" (Caqti_error.show e)) in
    Lwt.fail @@ Yaks_types.YException (`StoreError (`Msg (Caqti_error.show e)))

let exec_query conx query =
  let exec_query' (module C : Caqti_lwt.CONNECTION) =
    let%lwt _ = Logs_lwt.debug (fun m -> m "[SQL]: %s" query) in
    C.exec (Caqti_request.exec Caqti_type.unit query) ()
  in
  Caqti_lwt.Pool.use exec_query' conx.pool |> fail_on_error

let collect_query conx query row_type=
  let collect_query' (module C : Caqti_lwt.CONNECTION) =
    let%lwt _ = Logs_lwt.debug (fun m -> m "[SQL]: %s" query) in
    C.collect_list (Caqti_request.collect Caqti_type.unit row_type query) ()
  in
  Caqti_lwt.Pool.use collect_query' conx.pool |> fail_on_error




let database_type_of_url uri =
  match Uri.scheme uri with
  | None -> let err_msg = "no schema in URL "^(Uri.to_string uri) in print_endline ("ERROR: "^err_msg); failwith err_msg
  | Some scheme -> 
    if scheme = "postgresql" then POSTGRESQL
    else if scheme = "mariadb" then MARIADB
    else if scheme = "sqlite3" then SQLITE3
    else let err_msg = "Unkown schema in URL "^(Uri.to_string uri) in print_endline ("ERROR: "^err_msg); failwith err_msg


let connect url =
  let uri = Uri.of_string url in
  let db_type = database_type_of_url uri in
  match Caqti_lwt.connect_pool ~max_size:10 uri with
  | Ok pool -> { pool; db_type }
  | Error err -> let err_msg = Caqti_error.show err in print_endline ("ERROR: "^err_msg); failwith err_msg


module [@warning "-32"] Dyntype = struct
  type t = Pack : 'a Caqti_type.t * ('a -> string list) -> t

  let as_list x = x::[]
  let (<.>) f g = fun x -> f @@ g @@ x
  let empty = Pack (Caqti_type.unit, fun () -> [])
  let bool = Pack (Caqti_type.bool, as_list <.> string_of_bool)
  let int = Pack (Caqti_type.int, as_list <.> string_of_int)
  let int32 = Pack (Caqti_type.int32, as_list <.> Int32.to_string)
  let int64 = Pack (Caqti_type.int64, as_list <.> Int64.to_string)
  let float = Pack (Caqti_type.float, as_list <.> string_of_float)
  let string = Pack (Caqti_type.string, fun s -> s::[])
  let octets = Pack (Caqti_type.octets, fun s -> s::[])
  let pdate = Pack (Caqti_type.pdate, fun t -> (Ptime.to_date t |> function (y,m,d) -> Printf.sprintf "%d-%02d-%02d" y m d)::[])
  let ptime = Pack (Caqti_type.ptime, as_list <.> Ptime.to_rfc3339)
  let ptime_span = Pack (Caqti_type.ptime_span, as_list <.> string_of_float <.> Ptime.Span.to_float_s )

  let add (Pack (t,f)) (Pack (t',f')) = Pack ((Caqti_type.tup2 t t'), fun (v, v') ->  (f v)@(f' v'))

  let to_string (Pack (t,_)) = Caqti_type.show t
end

module TypeMap = Map.Make(String)

let types_map =
  let open TypeMap in
  TypeMap.empty |>
  (* PostgreSQL *)
  add "integer"   Dyntype.int |>
  add "real"      Dyntype.float |>
  add "character varying" Dyntype.string |>
  add "date"      Dyntype.pdate |>
  (*  MariaDB / MySQL *)
  add "bigint"  Dyntype.int |>
  add "int"  Dyntype.int |>
  add "double"  Dyntype.float |>
  add "varchar"  Dyntype.string |>
  add "date"  Dyntype.pdate |>
  (*  SQLite3 *)
  add "serial"  Dyntype.int |>
  add "int"  Dyntype.int |>
  add "real"  Dyntype.float |>
  add "varchar"  Dyntype.string |>
  add "date"  Dyntype.pdate


let normalized_type_name type_name =
  (match String.index_opt type_name '(' with
   | Some i -> String.sub type_name 0 i
   | None -> type_name)
  |> String.lowercase_ascii

let type_from_name type_name =
  match TypeMap.find_opt (normalized_type_name type_name) types_map with
  | Some t -> t
  | None -> failwith ("Unkown SQL type: "^type_name)

let schema_from_type_list type_list =
  let rec make_schema = function
    | t::[] -> type_from_name t
    | t::l -> Dyntype.add (type_from_name t) (make_schema l)
    | [] -> failwith "Cannot make schema for an empty type list"
  in
  make_schema type_list


let get_schema conx table_name =
  let query = match conx.db_type with
    | POSTGRESQL | MARIADB ->
      "SELECT column_name, data_type FROM information_schema.columns WHERE table_name = '"^table_name^"'"
    | SQLITE3 ->
      "SELECT name, type FROM pragma_table_info('"^table_name^"')"
  in
  match%lwt collect_query conx query Caqti_type.(tup2 string string) with
  | [] -> Lwt.return_none
  | schema -> List.split schema
              |> (fun (names, type_list) -> names, schema_from_type_list type_list)
              |> Lwt.return_some


let default_key_size = 3072   (* Note: this is max key size in MariaDB *)

let default_val_size = 1024*1024

let create_kv_table_query table_name props =
  let open Yaks_property in
  let open Apero.Option.Infix in
  let key_size = get_property Be_sql_property.Key.key_size props >>= int_of_string_opt >?= default_key_size in
  Caqti_request.exec
    Caqti_type.unit
    (Printf.sprintf
       "CREATE TABLE %s (k VARCHAR(%d) NOT NULL PRIMARY KEY, v TEXT)"
       table_name key_size)

let kv_table_schema = "k"::"v"::[], Dyntype.(add string string)

let create_kv_table conx table_name props =
  let open Yaks_property in
  let open Apero.Option.Infix in
  let key_size = get_property Be_sql_property.Key.key_size props >>= int_of_string_opt >?= default_key_size in
  let query = "CREATE TABLE "^table_name^" (k VARCHAR("^(string_of_int key_size)^") NOT NULL PRIMARY KEY, v TEXT)" in
  let open Apero.LwtM.InfixM in
  exec_query conx query >> Lwt.return kv_table_schema




let trunc_table conx table_name =
  let query = "TRUNCATE TABLE "^table_name in
  fun () -> exec_query conx query

let drop_table conx table_name =
  let query = "DROP TABLE "^table_name in
  fun () -> exec_query conx query

let get conx table_name ?condition (Dyntype.Pack (typ, typ_to_s)) =
  let open Lwt.Infix in
  let query = match condition with
    | Some cond -> "SELECT * FROM "^table_name^" WHERE "^cond
    | None -> "SELECT * FROM "^table_name
  in
  fun () -> collect_query conx query typ >|= (fun rows -> List.map (fun row -> typ_to_s row) rows)


let put conx table_name content =
  let query = "INSERT INTO "^table_name^" VALUES ("^content^")" in   (* TODO: content from JSON *)
  fun () -> exec_query conx query

let remove conx table_name condition =
  let query = "DELETE FROM "^table_name^" WHERE "^condition in
  fun () -> exec_query conx query

