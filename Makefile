.PHONY: all demo clean test doc

BUILD_LIB=dune build # --display=short
EXES=src/yaks/yaks-daemon/yaksd.exe
BUILD_EXE=dune build  ${EXES} # --display=short
CLEAN= dune clean
TEST=dune runtest -j1 --no-buffer
INSTALL=dune install
UNINSTALL = dune uninstall
all:
		${BUILD_LIB}
		${BUILD_EXE}
demo:
#	make -C demo

test:
		${TEST}

install:
	${INSTALL}

clean:
	${CLEAN}
#	make -C demo clean

uninstall:
	${UNINSTALL}

doc:
	dune build  @doc
