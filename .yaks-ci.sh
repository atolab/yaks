# sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
OPAM_DEPENDS="lwt dune logs fmt cmdliner cmdliner ppx_deriving uuidm yojson lwt_ppx lwt_log ppx_cstruct atdgen cohttp-lwt-unix"
WD=$PWD
eval `opam config env`
opam update
opam upgrade
opam install ${OPAM_DEPENDS}
cd ~
git clone -b master https://github.com/atolab/apero
make -C apero && make install -C apero 
cd $WD
make


