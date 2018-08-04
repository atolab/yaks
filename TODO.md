## Yaks Core

- Introduce the concept of identity and authentication which will represent the credentials used to:
  - Create an Access
  - Check access rights

## Yaks FE

- Add socket-based front-end

- Add support for subscriptions. The FE has to provide a way to the Engine to push
  notifications. This could be done by means of a function such as:
  
       push: Value.t -> unit Lwt.t. 

## YAKS BE

- Add DB backend

