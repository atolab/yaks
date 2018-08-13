open Yaks_core
open Lwt.Infix
open Yaks_sec
module DummySecurity = struct 

  module Make (MVar : Apero.MVar) = struct

  module U = Yaks_core.YUser
  module G = Yaks_core.YGroup
  module GroupMap = Map.Make(G.Id)
  module UserMap = Map.Make(U.Id) 

  type t = 
    { groups : G.t GroupMap.t
    ; users : U.t UserMap.t
  }

  let mvar_self = MVar.create {groups = GroupMap.empty; users = UserMap.empty}

  let create_group_with_id name rw_paths r_paths w_paths level group_id = 
    (* 
    Create a group with the parameters, return unit
   *)
    let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.create_group  id: %s " (G.Id.to_string group_id)) in
    let g = G.{id=group_id; name; rw_paths; r_paths; w_paths; group_level=level} in
    MVar.guarded mvar_self 
      (fun self ->  MVar.return () {self with groups = (GroupMap.add group_id g self.groups)})


  let create_group name rw_paths r_paths w_paths level = 
  (* 
    Create a group with the parameters, return the group id
   *)
    let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.create_group name: %s " name) in
    let uid = G.Id.next_id () in
    create_group_with_id name rw_paths r_paths w_paths level uid >|= fun () -> uid


  let dispose_group group_id = 
    MVar.guarded mvar_self 
      @@ fun (self:t) -> 
        (match GroupMap.mem group_id self.groups with
        | true -> 
              MVar.return () {self with groups = (GroupMap.remove group_id self.groups)}
        | false -> 
          MVar.return_lwt (Lwt.fail @@ YException (`InvalidParameters )) self)


  let create_user_with_id name password group user_id =
    (* 
      Create a new user with specgied id in the group identified by the group parameter
      return unit
     *)
    let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.create_user_with_id id: %s " (U.Id.to_string user_id)) in
    let u = U.{id=user_id; name; password; group } in
    MVar.guarded mvar_self 
      (fun self ->  MVar.return () {self with users = (UserMap.add user_id u self.users)})


  let create_user name password group = 
    (* 
      Create a new user in the group identified by the group parameter
      return the userid
     *)
    let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.create_user name: %s " name) in
    let uid = U.Id.next_id () in
    create_user_with_id name password group uid >|= fun () -> uid


  let authenticate_user name password =
    (* Authenticate an user based on username and password
      The creation of a token to manage the session is at front-end level
    *)
    let%lwt _ = Logs_lwt.debug (fun m -> m "Engine.authenticate_user name: %s " name) in
    MVar.read mvar_self >>= (fun e ->
    let b = UserMap.bindings e.users in
    let open U in
    let k,_ = List.find (fun (_,v) -> (v.name=name && v.password = password) ) b in
    Lwt.return k)


  let dispose_user user_id = 
    MVar.guarded mvar_self 
      @@ fun (self:t) -> 
        (match UserMap.mem user_id self.users with
        | true -> 
              MVar.return () {self with users = (UserMap.remove user_id self.users)}
        | false -> 
          MVar.return_lwt (Lwt.fail @@ YException (`InvalidParameters )) self)


  let get_user id =
    MVar.read mvar_self >>= 
      (fun s -> Lwt.return @@ UserMap.find_opt id s.users)


  let get_group id =
    MVar.read mvar_self >>= 
      (fun s -> Lwt.return @@ GroupMap.find_opt id s.groups )
    
  end
end

let make_dummy_security _ =
  let module M = DummySecurity.Make(Apero.MVar_lwt) in 
  (module M : Security)


module SecurityFactory  = struct 
  let make (ps:Property.t list)  = make_dummy_security ps
  let name = yaks_auth
end