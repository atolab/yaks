open Apero
open Lwt.Infix
open Yaks_types
open Yaks_core_types
open Yaks_be
open Yaks_storage
open Yaks_common_errors
open Influxdb_types

module InfluxBE = struct 

  module type Config = sig    
    val id: BeId.t 
    val properties : properties
    val base_url : string
  end

  module Make (C : Config) = struct
    let id = C.id
    let properties = C.properties
    let base_url = C.base_url

    let to_string = "InfluxBE#"^(BeId.to_string C.id)^"{"^(Properties.to_string properties)^"}"


    let regex_of_selector sel =
      (* See https://docs.influxdata.com/influxdb/v1.7/query_language/data_exploration/#regular-expressions 
         We replace each '*' with ".*" and each '/' with "\/".
         And surround the result with '/'.
      *)
      let stars_regex = Str.regexp "\\*\\*?" in
      let slash_regex = Str.regexp "/" in
      Selector.path sel |>
      Str.global_replace stars_regex ".*" |>
      Str.global_replace slash_regex "\/" |>
      Printf.sprintf "/%s/"

    let get storage_info selector =
      Logs.debug (fun m -> m "[Inflx]: get(%s) from db %s" (Selector.to_string selector) (storage_info.db.name));
      match Selector.remaining_after_match storage_info.keys_prefix selector with
      | None -> Lwt.return []
      | Some sub_sel -> 
        let measurement_regex = regex_of_selector sub_sel in
        Influxdb_driver.query_keyvalues storage_info.db ("SELECT * FROM "^measurement_regex) >>=
        Lwt_list.fold_left_s (fun result (key, values) ->
          (* NOTE: replacing '*' with '.*' in FROM clause gives more keys than we want (as .* is similar to .*.* ). 
            We need to check matching with sub-selector, and to discard those that don't match *)
          if Selector.is_matching_path (Path.of_string key) sub_sel then
            let path = (Path.of_string @@ (Path.to_string storage_info.keys_prefix)^key) in
            let pvs = List.map (fun tv -> (path, tv)) values in
            Lwt.return (pvs @ result)
          else
            Lwt.return result
        ) []

    let put storage_info path (value:TimedValue.t) =
      Logs.debug (fun m -> m "[Inflx]: put(%s) into db %s" (Path.to_string path) (storage_info.db.name));
      let measurement = measurement_from_path storage_info path in
      Influxdb_driver.write storage_info.db measurement value

    let update storage_info path (delta:TimedValue.t) =       
      ignore storage_info; ignore path; ignore delta;
      Logs.err (fun m -> m "[Inflx]: update not supported !!");
      Lwt.return_unit

    let remove storage_info path time = 
      ignore storage_info; ignore path; ignore time;   
      Logs.err (fun m -> m "[Inflx]: remove not supported !!");
      Lwt.return_unit


    let dispose storage_info () =
      match storage_info.on_dispose with
      | DropDB -> 
        Logs.debug (fun m -> m "[Inflx]: dispose storage on db %s dropping it" storage_info.db.name);
        Influxdb_driver.query storage_info.db ~post:true ("DROP DATABASE "^storage_info.db.name)
        >>= fun _ -> Lwt.return_unit
      | DropAllSeries ->
        Logs.debug (fun m -> m "[Inflx]: dispose storage on db %s dropping all its series" storage_info.db.name);
        Influxdb_driver.query storage_info.db ~post:true ("DROP SERIES FROM /.*/")
        >>= fun _ -> Lwt.return_unit
      | DoNothing ->
        Logs.debug (fun m -> m "[Inflx]: dispose storage on db %s keeping it" storage_info.db.name);
        Lwt.return_unit 

    let make_db_name () =
      "Yaks_db_"^(Uuid.make () |> Uuid.to_string |> String.map (function | '-' -> '_' | c -> c))

    let create_storage selector props =
      let props = Properties.union (fun _ _ v2 -> Some v2) properties props in
      let (db_name, props) = match Properties.get "db" props with
        | Some name -> (name, props)
        | None -> let name = make_db_name () in (name, Properties.add "db" name props)
      in
      Logs.debug (fun m -> m "[Infx] create_storage on %s in db '%s' with %s" (Selector.to_string selector) db_name (Properties.to_string props));
      let%lwt db = Influxdb_driver.get_db base_url db_name in
      let keys_prefix = Selector.get_prefix selector in
      let keys_prefix_length = String.length (Path.to_string keys_prefix) in
      let on_dispose = on_dispose_from_properties props in
      let storage_info = { selector; keys_prefix; keys_prefix_length; props; db; on_dispose} in
      Lwt.return @@ Storage.make selector props
        (dispose storage_info)
        (get storage_info)
        (put storage_info)
        (update storage_info)
        (remove storage_info)
  end
end


module InfluxBEF = struct
  let make id properties =
    let url = match Properties.get "url" properties with
      | Some url -> url
      | None -> raise @@ YException (`InvalidBackendProperty (`Msg ("Property 'url' is not specified")))
    in
    let%lwt () = Influxdb_driver.ping url in
    let module M = InfluxBE.Make (
    struct 
      let id = id
      let properties = Properties.add "kind" "time-series" properties
      let base_url = url
    end) in
    Lwt.return (module M : Backend)

end

let () =
  Yaks_be.register_backend_factory (module InfluxBEF:BackendFactory);
