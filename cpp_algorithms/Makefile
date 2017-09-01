
CLANG=clang
FLAGS=-lstdc++ -Werror


# quicksort: quicksort.cpp
# 	@echo 'Compiling quicksort.cpp...'
# 	@$(CLANG) -o $@ $< $(FLAGS)
# 	@./$@


%: %.c
	@echo "Compiling $@..."
	@$(CLANG) -o $@ $< $(FLAGS)
	@./$@


all: quicksort
.PHONY: all
