default: native lib.a
	mv swamp.native swamp
	
native: 
	ocamlbuild -pkgs llvm swamp.native

test: clean default 
	sh autotest.sh -s

lib.a: lib.o 
	ar -crs liball.a lib.o

lib.o: lib.c lib.h

parser.native: parser.mly ast.mli scanner.mll
	ocamlbuild parser.native

scanner.native: scanner.mll
	ocamlbuild scanner.native

##############################


.PHONY : clean
clean :
	ocamlbuild -clean 2>/dev/null
	rm -rf *.cmi *.cmo 
	rm -rf parser.ml parser.mli scanner.ml test-* bad-* testall.log 
	rm -rf *.o *.s *.a *.native *.output
	rm -rf _build

.PHONY: all
all: clean default
