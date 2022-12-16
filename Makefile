native: clean
	ocamlbuild -pkgs llvm swamp.native

test: clean native 
	sh autotest.sh -s

%.cmo : %.ml
	ocamlc -w +A-e-l-z -c $<

%.cmi : %.mli
	ocamlc -w +A-e-l-z -c $<

scanner.ml : scanner.mll
	ocamllex $^

parser.ml parser.mli : parser.mly
	ocamlyacc -v $^

# Depedencies from ocamldep
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmo parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx


##############################


.PHONY : clean
clean :
	rm -rf *.cmi *.cmo parser.ml parser.mli scanner.ml test-* bad-* testall.log *.native *.output
