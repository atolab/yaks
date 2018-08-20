.PHONY: all demo clean test doc

BUILD_LIB=dune build --only-packages=yaks-core,yaks-sec-dum,yaks-be-mm,yaks-fe-rest,yaks-fe-sock,yaks
EXES=src/yaks/yaks-daemon/yaksd.exe #src/yaks-tools/yaks-cat/yaksc.exe
BUILD_EXE=dune build ${EXES}
CLEAN= dune clean
TEST=dune runtest -j1 --no-buffer --dev
INSTALL=dune install --only-packages=yaks-core,yaks-sec-dum,yaks-be-mm,yaks-fe-rest,yaks-fe-sock,yaks
UNINSTALL = dune uninstall --only-packages=yaks-core,yaks-sec-dum,yaks-be-mm,yaks-fe-rest,yaks-fe-sock,yaks
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
