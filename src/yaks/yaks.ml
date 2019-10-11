open Lwt.Infix
open Apero
open Yaks_types
open Yaks_core_types
open Yaks_common_errors
open Yaks_be
open Yaks_storage
open Cmdliner


module BackendMap = Map.Make (BeId)
module StorageMap  = Map.Make (StorageId)
module KVMap = Map.Make(Path)

type backend =
  { beModule : (module Backend)
  ; storages : (Storage.t * Zenoh.storage) StorageMap.t
  }

type state = 
  { zenoh: Zenoh.t
  ; admin_prefix: string
  ; backends : backend BackendMap.t
  ; kvs : TimedValue.t KVMap.t
  }

type t = state Guard.t

let kvmap_add path value time =
  let tv: TimedValue.t = { time; value } in
  KVMap.add path tv

let update_yaks_json_view prefix time kvs =
  let rec add_to_json (json:Yojson.Basic.t) (path:string list) (v:Yojson.Basic.t) =
    match json, path with
    | `Assoc l , [] -> `Assoc l
    | `Assoc l , p::[] -> `Assoc ((p, v)::l)
    | `Assoc l , p::tl -> (match List.find_opt (fun (s,_) -> s = p) l with
      | Some (_,j) -> let l' = List.filter (fun (s,_) -> s <> p) l in `Assoc ((p, add_to_json j tl v)::l')
      | None -> `Assoc ((p, add_to_json (`Assoc []) tl v)::l)
    )
    | _, _ -> failwith ("INTERNAL ERROR: Unexpected JSON type reconstructing value for "^prefix)
  in
  let selector = Selector.of_string @@ prefix^"/**" in
  let keys = KVMap.filter (fun path _ -> (not @@ (Path.to_string path = prefix)) && Selector.is_matching_path path selector) kvs
    |> KVMap.bindings
    |> List.map (fun (p,(v:TimedValue.t)) ->
        let subpath = Path.remove_prefix (String.length prefix +1) p |> Path.to_string in
        match Value.transcode v.value Value.JSON with
        | Ok js -> let json = Yojson.Basic.from_string @@ Value.to_string js in (String.split_on_char '/' subpath, json)
        | Error _ -> failwith ("INTERNAL ERROR: failed to transcode to JSON the value of  "^(Path.to_string p)))
  in
  let json = List.fold_left (fun json (path, v) -> add_to_json json path v)
    (`Assoc []) keys |> Yojson.Basic.to_string in
  kvmap_add (Path.of_string prefix) (Value.JSonValue json) time kvs


let sep = Filename.dir_sep
let exe_dir = Filename.dirname Sys.executable_name

let possible_filename bename =
  if Astring.is_suffix ~affix:".cmxs" bename then
    [
      bename;
      "yaks-be-" ^ bename;
    ]
  else
    [
      bename ^ ".cmxs";
      "yaks-be-" ^bename ^ ".cmxs";
    ]


let lookup_file filename =
  List.find_opt (fun file -> Sys.file_exists file)
  [
    filename;
    exe_dir ^ sep ^ ".." ^ sep ^ "lib" ^ sep ^ filename;
    "~/.yaks/lib/" ^ filename;
    "/usr/local/lib/" ^ filename;
    "/usr/lib/" ^ filename;
  ]

let lookup_be_lib bename =
  let rec lookup filenames =
    match filenames with
    | [] -> None
    | filename::filenames ->
      match lookup_file filename with
      | Some file -> Some file
      | None -> lookup filenames
  in
  lookup @@ possible_filename bename


(**************************)
(*   Backends management  *)
(**************************) 
let add_loaded_backend state be time = 
  let module BE = (val be: Backend) in
  let id = BeId.to_string BE.id in
  let backend = { beModule = be; storages = StorageMap.empty } in
  Logs.debug (fun m -> m "[Yaks] add_backend : %s" id);
  let kvs = kvmap_add
    (Path.of_string @@ Printf.sprintf "%s/backend/%s/" state.admin_prefix id)
    (Value.PropertiesValue BE.properties) time state.kvs
    |> update_yaks_json_view state.admin_prefix time
  in
  { state with backends = BackendMap.add BE.id backend state.backends; kvs }

let add_backend t beid properties time =
  let id = BeId.of_string beid in
  Guard.guarded t
  @@ fun self ->
  if BackendMap.mem id self.backends then
    Guard.return_lwt (Lwt.fail @@ YException (`Forbidden (`Msg ("Already existing backend: "^beid)))) self
  else
    let lib = Properties.get_or_default "lib" ~default:beid properties in
    match lookup_be_lib lib with
    | Some file -> begin
      Logs.debug (fun m -> m "[Yaks] load backend %s with lib=%s : library found at %s" beid lib file );
      Logs.debug (fun m -> m "[Yaks] load backend %s with properties: %s" beid (Properties.to_string properties));
      try
        begin
          Dynlink.loadfile @@ Dynlink.adapt_filename file;
          let module BEF = (val Yaks_be.get_loaded_backend_factory () : BackendFactory) in
          let module BE = (val BEF.make (BeId.of_string beid) properties : Backend) in
          Guard.return () @@ add_loaded_backend self (module BE) time
        end
      with
        | Dynlink.Error s -> failwith (Printf.sprintf "Error loading backend %s from %s: %s" beid lib (Dynlink.error_message s))
      end
    | None -> failwith (Printf.sprintf "Error loading backend %s with lib=%s: file not found" beid lib)

let remove_backend t beid time = 
  ignore t; ignore beid; ignore time;
  Lwt.fail @@ YException (`InternalError (`Msg ("remove_backend not yet implemented")))

(**************************)
(*   Storages management  *)
(**************************)
let find_compatible_backend backends properties =
  BackendMap.filter (fun beid be ->
    let module BE = (val be.beModule: Backend) in
    Logs.debug (fun m -> m "[Yaks]:    try Backend %s (%s)" (BeId.to_string beid) BE.to_string);
    Properties.not_conflicting properties BE.properties) backends
  |> BackendMap.choose_opt

let create_storage t ?beid stid properties time =
  Logs.debug (fun m -> m "[Yaks] create_storage %s with %s" stid (Properties.to_string properties));
  (* get "selector" from properties *)
  let%lwt selector = match Properties.find_opt "selector" properties with
    | None -> Lwt.fail @@ YException (`InternalError (`Msg ("create_storage "^stid^" without 'selector' in properties")))
    | Some s -> (match Selector.of_string_opt s with 
      | None -> Lwt.fail @@ YException (`InternalError (`Msg ("create_storage "^stid^" with invalid 'selector': "^s)))
      | Some s' -> Lwt.return s')
  in
  let storageId = StorageId.of_string stid in
  Logs.debug (fun m -> m "[Yaks] create_storage %s on %s" stid (Selector.to_string selector));
  Guard.guarded t
  @@ fun self ->
  (* get backend from beid or a compatible one if beid is not set *)
  let%lwt (beid', be) = match beid with
    | None -> (match find_compatible_backend self.backends properties with
      | Some (id, be) -> Lwt.return (id, be)
      | None -> Lwt.fail @@ YException (`InternalError (`Msg ("create_storage "^stid^" failed: no comatible backend ")))
      )
    | Some s -> let id = BeId.of_string s in
      (match BackendMap.find_opt id self.backends with
      | Some be -> Lwt.return (id, be)
      | None -> Lwt.fail @@ YException (`InternalError (`Msg ("create_storage "^stid^" failed: backend not found: "^s)))
    )
  in
  (* check if storage with this id doent's already exists*)
  if StorageMap.mem storageId be.storages then
    Lwt.fail @@ YException (`InternalError (`Msg (
      Printf.sprintf "create_storage %s on %s failed: storage with same id already exists" stid (BeId.to_string beid'))))
  else
    (* create storage (in backend and in Zenoh) and add it to self state *)
    let module BE = (val be.beModule: Backend) in
    Logs.debug (fun m -> m "[Yaks]: create_storage %s using Backend %s" stid (BE.to_string));
    let%lwt storage = BE.create_storage selector properties in
    let%lwt zenoh_storage =
      let open LwtM.InfixM in
      Storage.align storage self.zenoh selector >>
      lazy (Yaks_zenoh_utils.store self.zenoh selector (Storage.on_zenoh_write storage) (Storage.get storage))
    in
    let be' = { be with storages = StorageMap.add storageId (storage, zenoh_storage) be.storages } in
    let kvs = kvmap_add
      (Path.of_string @@ Printf.sprintf "%s/backend/%s/storage/%s" self.admin_prefix (BeId.to_string beid') stid)
      (Value.PropertiesValue properties) time self.kvs
      |> update_yaks_json_view self.admin_prefix time
    in
    Guard.return () { self with backends = BackendMap.add beid' be' self.backends; kvs }

let remove_storage t beid stid time =
  let beid' = BeId.of_string beid in
  let stid' = StorageId.of_string stid in
  Logs.debug (fun m -> m "[Yaks] remove_storage %s/%s" beid stid);
  Guard.guarded t
  @@ fun self ->
  let%lwt be = match BackendMap.find_opt beid' self.backends with
    | None -> Lwt.fail @@ YException (`InternalError (`Msg ("No backend with id: "^beid)))
    | Some be -> Lwt.return be
  in
  match StorageMap.find_opt stid' be.storages with
  | Some (storage, zenoh_storage) ->
    let%lwt _ = Yaks_zenoh_utils.unstore self.zenoh zenoh_storage in
    Storage.dispose storage >>= fun () ->
    let be' = { be with storages = StorageMap.remove stid' be.storages } in
    let kvs = KVMap.remove (Path.of_string @@ Printf.sprintf "%s/backend/%s/storage/%s" self.admin_prefix beid stid) self.kvs
      |> update_yaks_json_view self.admin_prefix time
    in
    Guard.return () { self with backends = BackendMap.add beid' be' self.backends; kvs }
  | None -> 
    Logs.debug (fun m -> m "[Yaks] storage %s/%s not found... ignore remove request" beid stid);
    Guard.return () self

(*****************************)
(* get/put/remove operations *)
(*****************************)
let get t selector =
  Logs.debug (fun m -> m "[Yaks]: get %s" (Selector.to_string selector));
  let self = Guard.get t in 
  Lwt.return @@ match Selector.as_unique_path selector with 
  | Some path ->
    (match KVMap.find_opt path self.kvs with 
    | Some v -> [(path, v)]
    | None -> [])
  | None -> 
    self.kvs
      |> KVMap.filter (fun path _ -> Selector.is_matching_path path selector)
      |> KVMap.bindings

let put t path (tv:TimedValue.t) =
  Logs.debug (fun m -> m "[Yaks]: put %s : %s" (Path.to_string path) (Value.to_string tv.value));
  let time = tv.time in
  let%lwt properties = 
    let open Value in
    match transcode tv.value JSON with
    | Ok JSonValue json ->
      begin
        match transcode (JSonValue json) PROPERTIES with
        | Ok PropertiesValue p -> Lwt.return p
        | Ok _ -> Lwt.fail @@ YException (`UnsupportedTranscoding (`Msg "Transcoding to Properties didn't return a PropertiesValue"))
        | Error e -> Lwt.fail @@ YException e
      end
    | Ok _ -> Lwt.fail @@ YException (`UnsupportedTranscoding (`Msg "Transcoding to Json didn't return a JsonValue"))
    | Error e -> Lwt.fail @@ YException e
  in
  let self = Guard.get t in 
  if Astring.is_prefix ~affix:self.admin_prefix (Path.to_string path) then
    match String.split_on_char '/' @@ Astring.with_range ~first:(String.length self.admin_prefix+1) (Path.to_string path) with
    | ["backend"; "auto"]  -> Lwt.fail @@ YException (`InvalidPath (`Msg ("Creation of backend/auto forbidden (reserved id)")))
    | ["backend"; beid]  -> add_backend t beid properties time
    | ["backend"; "auto"; "storage"; stid] -> create_storage t stid properties time
    | ["backend"; beid; "storage"; stid] -> create_storage t ~beid stid properties time
    | _ -> Lwt.fail @@ YException (`InvalidPath (`Msg ("Invalid path on admin space: "^(Path.to_string path))))
  else
    Lwt.fail @@ YException (`InternalError (`Msg ("put on remote Yaks admin not yet implemented")))

let remove t path time =
  Logs.debug (fun m -> m "[Yaks]: remove %s" (Path.to_string path));
  let self = Guard.get t in 
  if Astring.is_prefix ~affix:self.admin_prefix (Path.to_string path) then
    match String.split_on_char '/' @@ Astring.with_range ~first:(String.length self.admin_prefix+1) (Path.to_string path) with
    | ["backend"; beid]  -> remove_backend t beid time
    | ["backend"; beid; "storage"; stid] -> remove_storage t beid stid time
    | _ -> Lwt.fail @@ YException (`InvalidPath (`Msg ("Invalid path on admin space: "^(Path.to_string path))))
  else
    Lwt.fail @@ YException (`InternalError (`Msg ("remove on remote Yaks admin not yet implemented")))



(*****************************)
(* initialisation operations *)
(*****************************)
let memory_beid = "Memory"

let add_memory_backend t =
  Guard.guarded t
  @@ fun self ->
    Logs.debug (fun m -> m "[Yaks] add memory backend ");
    let module BE = (val Yaks_be_mm.MainMemoryBEF.make (BeId.of_string memory_beid) Properties.empty : Backend) in
    Guard.return () @@ add_loaded_backend self (module BE) Yaks_zenoh_utils.timestamp0

let add_default_storage t =
  Logs.debug (fun m -> m "[Yaks] add default memory storage on /** ");
  let props = Properties.singleton "selector" "/**" in
  create_storage t ~beid:memory_beid "default" props Yaks_zenoh_utils.timestamp0


let no_backend = Arg.(value & flag & info ["n"; "no-backend"] ~docv:"true|false" 
  ~doc:"If true, Yaks is started without any backend (nor storage). If false (default) Yaks loads the Memory backend at startup.")
let without_storage = Arg.(value & flag & info ["w"; "without-storage"] ~docv:"true|false" 
  ~doc:"If true, disable the creation at startup of a Memory backend and of a default memory storage with '/**' as selector.")

let () = 
  Logs.debug (fun m -> m "[Yaks] starting with args: %s\n%!" (Array.to_list Sys.argv |> String.concat " "));
  let run no_backend without_storage =
    Lwt.async @@ fun () ->
    let%lwt zenoh = Zenoh.zopen "" in
    let zprops = Zenoh.info zenoh in
    let zpid = match Properties.get "peer_pid" zprops with
      | Some pid -> pid
      | None -> Uuid.make () |> Uuid.to_string
    in
    let admin_prefix = "/@/"^zpid^"/plugins/yaks" in
    let (t:t) = Guard.create { zenoh; admin_prefix; backends=BackendMap.empty;  kvs=KVMap.empty } in
    Logs.info (fun m -> m "[Yaks] create Yaks admin space on %s/**" admin_prefix);
    let on_changes path changes =
      Lwt_list.iter_s (function
        | Put(tv)      -> put t path tv
        | Remove(time) -> remove t path time
        | Update(_)    -> Logs.warn (fun m -> m "[Yaks]: Received update for %s : only put or remove are supported by Admin space" (Path.to_string path)); Lwt.return_unit
      ) changes
    in
        Yaks_zenoh_utils.store zenoh (Selector.of_string @@ admin_prefix^"/**") on_changes (get t)
        >>= (fun _ -> if not no_backend then add_memory_backend t  else Lwt.return_unit)
        >>= (fun _ -> if not no_backend && not without_storage then add_default_storage t else Lwt.return_unit)
  in
  let _ = Term.(eval ~argv:Sys.argv (const run $ no_backend $ without_storage, Term.info "yaksd"))
  in ()
