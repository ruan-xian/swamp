all : clean calc.out

native: clean
	ocamlbuild -pkgs llvm swamp.native

test: clean calc 
	sh run_tests.sh

print_test: clean build_print_test
	sh run_print_tests.sh

build_print_test: 
	ocamlbuild print_test.native

semant_test: clean build_semant_test
	sh run_semant_tests.sh

build_semant_test:
	ocamlbuild -pkgs llvm swamp.native

calc : parser.cmo scanner.cmo calc.cmo
	ocamlc -w +A-e-l-z -o calc $^

%.cmo : %.ml
	ocamlc -w +A-e-l-z -c $<

%.cmi : %.mli
	ocamlc -w +A-e-l-z -c $<

scanner.ml : scanner.mll
	ocamllex $^

parser.ml parser.mli : parser.mly
	ocamlyacc -v $^

calc.out : calc calc.tb
	./calc < calc.tb > calc.out

# Depedencies from ocamldep
calc.cmo : scanner.cmo parser.cmi ast.cmo
calc.cmx : scanner.cmx parser.cmx ast.cmo
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmo parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx


##############################


.PHONY : clean
clean :
	rm -rf *.cmi *.cmo parser.ml parser.mli scanner.ml calc.out calc print_test.native
