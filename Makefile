SRC = src/parser.scm src/main.scm src/compiler.scm src/emitter.scm
OBJ = $(SRC:.scm=.o)
CSCFLAGS = -static
#CSCFLAGS =

all: compiler

compiler: $(OBJ)
	chicken-csc $(OBJ) $(CSCFLAGS) -o $@

%.o: %.scm
	chicken-csc $(CSCFLAGS) -c $<

.PHONY: test
test: compiler
	for thing in tests/src/*.c; do \
		echo "TEST $$thing"; \
		./compiler $$thing; \
	done

.PHONY: clean
clean:
	rm -f $(OBJ) compiler
