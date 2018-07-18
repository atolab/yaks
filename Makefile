.PHONY: all demo clean test doc

BUILD_LIB=jbuilder build
EXES=src/yaks/yaks-daemon/yaksd.exe #src/yaks-tools/yaks-cat/yaksc.exe
BUILD_EXE=jbuilder build ${EXES}
CLEAN= jbuilder clean
TEST=jbuilder runtest -j1 --no-buffer --dev
INSTALL=jbuilder install

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

doc:
	jbuilder build  @doc
