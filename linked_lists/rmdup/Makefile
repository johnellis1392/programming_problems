
BIN=rmdup
FLAGS=-Werror

compile: ${BIN}.c
	@clang -o ${BIN} $< ${FLAGS}

run: compile
	@./${BIN}

all: run

