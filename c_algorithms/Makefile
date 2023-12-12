
CLANG=clang
FLAGS=-Werror

%: %.c
	@echo "Compiling $@..."
	@$(CLANG) -o $@ $< $(FLAGS)
	@./$@
