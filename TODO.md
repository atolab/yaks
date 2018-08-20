## Yaks Core

- Introduce the concept of identity and authentication which will represent the credentials used to:
  - ~Create an Access~
  - ~Check access rights~
  - ~Create user~
  - ~Create group~
  - ~Authenticate~
  - ~Delete User~
  - ~Delete Group~
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

- REST FE:
  - ~Deauthenticate~
  - ~create access, remove access,get, put, delta_put should be used only after authetication~
  - Create storage, remove storage, create group, create user should be used only after authetication of an Admin user

## YAKS BE

- Add DB backend

## MirageOS

- ~Try to have Yaks with REST FE running on MirageOS (Gabriele can you give it a try?)~ 
  
Is running fine on MirageOS as Unix Process, see in ocaml-examples

If trying to build an image using ukvm or the virtio this error raises
```
Error: No implementations provided for the following modules:
         Str referenced from /home/ubuntu/.opam/4.06.0/lib/yaks-core/yaks_core.cmxa(Yaks_types),
           /home/ubuntu/.opam/4.06.0/lib/yaks-fe-rest/yaks_fe_rest.cmxa(Yaks_fe_rest_mvar)
         Unix referenced from /home/ubuntu/.opam/4.06.0/lib/lwt/unix/lwt_unix.cmxa(Lwt_engine),
           /home/ubuntu/.opam/4.06.0/lib/lwt/unix/lwt_unix.cmxa(Lwt_unix),
           /home/ubuntu/.opam/4.06.0/lib/lwt/unix/lwt_unix.cmxa(Lwt_io),
           /home/ubuntu/.opam/4.06.0/lib/ipaddr/unix/ipaddr_unix.cmxa(Ipaddr_unix),
           /home/ubuntu/.opam/4.06.0/lib/conduit-lwt-unix/conduit_lwt_unix.cmxa(Conduit_lwt_server),
           /home/ubuntu/.opam/4.06.0/lib/conduit-lwt-unix/conduit_lwt_unix.cmxa(Conduit_lwt_unix),
           /home/ubuntu/.opam/4.06.0/lib/cohttp-lwt-unix/cohttp_lwt_unix.cmxa(Cohttp_lwt_unix__Server)
```
~A good replacement for `Str` should be [ocaml-re](https://github.com/ocaml/ocaml-re)  (See this [Issue on MirageOS](https://github.com/mirage/mirage/issues/901) )~

~While instead of `Cohttp-lwt-unix` we should use `Cohttp-lwt` because provide a OS-indipendent interface (See the repo for details [Ocaml-Cohttp](https://github.com/mirage/ocaml-cohttp))~

See [FE REST Mirage](https://github.com/atolab/yaks/blob/yaks_mvar/src/yaks-fe/yaks-fe-rest-mirage/yaks_fe_rest_mirage.ml) for the Front-End

After those updates the build still fails:

```
File "_none_", line 1:
Error: No implementations provided for the following modules:
         Unix referenced from /home/ato/.opam/4.06.0/lib/lwt/unix/lwt_unix.cmxa(Lwt_engine),
           /home/ato/.opam/4.06.0/lib/lwt/unix/lwt_unix.cmxa(Lwt_unix),
           /home/ato/.opam/4.06.0/lib/lwt/unix/lwt_unix.cmxa(Lwt_io)
```
Because Lwt_unix,Lwt_io module are used also by Apero, this mean that also in apero we have to migrate to something not unix-depend.

Should be possible to replace:
  - Lwt_io <-> [Mirage-Channel](https://github.com/mirage/mirage-channel) and [Mirage-Flow](https://github.com/mirage/mirage-flow)
  - Lwt_unix 
    - Network addresses <-> [Ocaml-ipaddr](https://github.com/mirage/ocaml-ipaddr) and [Mirage-tcpip](https://github.com/mirage/mirage-tcpip)
    - Sleep <->  Time.sleep_ns built-in in OS [Mirage-Platform](https://github.com/mirage/mirage-platform)
    - File descriptors 
      - Sockets <-> [Mirage-tcpip](https://github.com/mirage/mirage-tcpip)
      - Configuration files <-> [Mirage-kv](https://github.com/mirage/mirage-kv)
      - File system  <-> Mirage-Kv can be also used in this case, there are no modules that exposes reading on a filesystem

An example of Mirage-channel and Mirage-Flow can be found (here)[https://github.com/atolab/ocaml-examples/tree/master/mirage-os/cflow]