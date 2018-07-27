module Ypath = struct

  type t = string

  (* from https://tools.ietf.org/html/rfc3986#appendix-B *)
  let path_regex = Str.regexp "[^?#]*"

  let is_valid s = Str.string_match path_regex s 0

  let of_string (s:string) : t = 
    if is_valid s then s else raise (Invalid_argument s)

  let to_string (s:t) : string = s

  let is_prefix prefix path = Apero.String.starts_with (to_string prefix) (to_string path)

  let matches a b = true

end