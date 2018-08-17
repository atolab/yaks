open Yaks_types

module Access = struct

  module Id = struct
    include String

    let make () = Apero.Uuid.next_id () |> Apero.Uuid.to_string
    let of_string ?pos s = 
      match pos with
      | Some(p) -> Some(String.sub s p (String.length s - p))
      | None -> Some(s)
    let to_string ?upper s = match upper with | Some(true) -> String.uppercase_ascii s | _ -> s
    let to_bytes = to_string ~upper:false
    let of_bytes = of_string

  end [@@deriving show]

  type access_right =  R_Mode | W_Mode | RW_Mode

  type t = 
    { id : Id.t 
    ; path : Path.t 
    ; cache_size : int64
    ; right : access_right } 

  let make path cache_size right = { id = Id.make(); path; cache_size; right }

  let make_with_id id path cache_size right = { id = id; path; cache_size; right }

  let id a = a.id
  let path a = a.path 
  let cache_size a = a.cache_size
  let right a = a.right

  let check_access_right _ _ _ = Lwt.return_unit 

end  [@@deriving show]

