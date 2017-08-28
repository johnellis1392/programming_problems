
BIN=vector_test
FLAGS=-lstdc++ -Werror
COMP=clang

clean:
	@rm -rf ${BIN} *.o *~

compile: ${BIN}.cpp
	@${COMP} -o ${BIN} $< ${FLAGS}

run: compile
	@./${BIN}

all: run
.PHONY: all run compile
