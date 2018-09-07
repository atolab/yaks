
open Yaks_user

(* At the present time the user is not authenticated and a 
default user-id is returned. In any case no security checks 
are currently performed thus this provide the right mechanism to
add the control logic when needed *)

let default_user = User.Id.make ()
let authenticate _ _ (* properties engine *) = default_user
  
  