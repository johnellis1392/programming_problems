
BIN=str_sanitize
FLAGS=-Werror

compile: ${BIN}.c
	@clang -o ${BIN} ${BIN}.c ${FLAGS}

run: compile
	@./${BIN}

all: run
