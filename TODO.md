## Yaks Core

- Introduce the concept of identity and authentication which will represent the credentials used to:
  - ~Create an Access~
  - ~Check access rights~
  - ~Create user~
  - ~Create group~
  - ~Authenticate~
  - ~Deauthenticate~
  - Delete User
  - Delete Group
  - ~get, put, delta_put should be used only after authetication~
  - User types? (Now we have only Admin, Users, we may need operator?
  - Only Admin can create storages, groups and users
  - Only Admin can add/remove writing/reading rights from a group (this should also update all the access that have been created)
  - Store users/groups
## Yaks FE

- Add socket-based front-end

- Add support for subscriptions. The FE has to provide a way to the Engine to push
  notifications. This could be done by means of a function such as:
  
       push: Value.t -> unit Lwt.t. 

## YAKS BE

- Add DB backend

