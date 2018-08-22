open Yaks_types
open Yaks_property
open Yaks_user
open Yaks_group
open Yaks_access

module type Security = sig 

  type t

  val create_group : string -> Selector.t list -> Selector.t list -> Selector.t list -> Group.group_level -> Group.Id.t Lwt.t
  val create_group_with_id : string -> Selector.t list -> Selector.t list -> Selector.t list -> Group.group_level -> Group.Id.t -> unit Lwt.t
  val dispose_group : Group.Id.t -> unit Lwt.t 

  val create_user : string -> string -> Group.Id.t -> User.Id.t Lwt.t
  val create_user_with_id : string -> string -> Group.Id.t -> User.Id.t -> unit Lwt.t
  val authenticate_user : string -> string -> User.Id.t Lwt.t
  val dispose_user : User.Id.t -> unit Lwt.t 

  val get_user : User.Id.t -> User.t option Lwt.t
  val get_group : Group.Id.t -> Group.t option Lwt.t

  (* @TODO: This check should also verify that the user is in the right groups 
      Nasty users can use access coming from an authorized user to get access to data
      that they may not have access rights.
      So the check should be at access level and user level
      GB: We can store the user id in the access, in this way an user is coupled to one or more access, but an access can be used only by
      the user that created that access
     *)
  val check_write_access : Access.t -> Selector.t -> unit Lwt.t
  val check_read_access : Access.t -> Selector.t -> unit Lwt.t

  val get_access_creation_rights : Group.t -> Path.t -> (Access.access_right, yerror) Common.Result.t  

end

module type SecurityFactory  = sig 
  val make : Property.t list  -> (module Security)
  val name : string
end