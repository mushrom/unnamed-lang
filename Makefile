SRC = src/parser.scm src/compile.scm
OBJ = $(SRC:.scm=.o)
CSCFLAGS = -static
#CSCFLAGS =

all: compiler

compiler: $(OBJ)
	chicken-csc $(OBJ) $(CSCFLAGS) -o $@

%.o: %.scm
	chicken-csc $(CSCFLAGS) -c $<

lextest: lextest.scm
	chicken-csc -static lextest.scm

partest: partest.scm
	chicken-csc -static partest.scm

.PHONY: clean
clean:
	rm -f $(OBJ) compiler
