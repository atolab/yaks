#sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
OPAM_DEPENDS="lwt dune logs fmt cmdliner cmdliner ppx_deriving uuidm yojson lwt_ppx lwt_log ppx_cstruct atdgen cohttp-lwt-unix"
sh .travis-opam.sh

