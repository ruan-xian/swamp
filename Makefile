default: native irgen.a
	mv swamp.native swamp
	
native: clean 
	ocamlbuild -pkgs llvm swamp.native

test: clean default 
	sh autotest.sh

irgen.a: irgen.o 
	ar -crs irgen.a irgen.o

irgen.o: irgen.c irgen.h

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
	rm -rf *.o *.s *.a *.native *.output *.ll *.out *.diff
	rm -rf _build

.PHONY: all
all: clean default
