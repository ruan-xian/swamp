all : clean calc.out

native: 
	ocamlbuild -pkgs llvm swamp.native

##############################
#
# HW 2: Question 1
#
# Compilation: 
# Option 1: Simply type "make" to compile the calculator (recommended, auto-test included)
# Option 2: "ocamlbuild calc.native" will also build the calculator

# For testing, you can run the binary executable and test it with
# standard input via terminal.
# Or use calc.tb (testbench file): you can modify the file directly
# with the exprssion you want to test before make. After compiling
# your executable successfully, the output of test case will be 
# generate automatically in a file named calc.out

test: clean calc 
	sh run_tests.sh

print_test: clean build_print_test
	sh run_print_tests.sh

build_print_test: 
	ocamlbuild print_test.native

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
