0=|				SWAMP README				|=0
		Ellen Guo (ekg2134) - System Architect
		Ken Miura (km3635) - Language Guru
		Alice Wang (aw3271) - Manager
		Ryan Xu (rx2189) - Tester
		Cindy Zhu (cwz2102) - Tester

To use:
make 											generates the swamp compiler
./swamp example.swamp > example.out				writes out llvm code (no C functions here)
./compile_swamp.sh example.swamp				compiles to example.exe; C functions properly linked
./autotest.sh [-sk]								runs all tests (or specified tests). -s for SAST mode, -k to keep intermediate files
./autotest.sh [-sk] example.swamp				runs only the test for this Swamp file

If you want to manually compile Swamp LLVM to C-linked Swamp, check the contents of compile_swamp.sh.

Files:
	ast.ml										abstract syntax tree definition
	autotest.sh									automated testing script
	compile_swamp.sh							compiles and links swamp code
	irgen.c 									C functions for Swamp
	irgen.h 									header for irgen.c 
	irgen.ml									IR code generator
	Makefile 									Makefile
	parser.mly									parser
	sast.ml 									semantically checked abstract syntax tree definition
	scanner.mll 								lexer
	semant.ml 									semantic checker
	swamp.ml									compiler
	test_cases/									test cases and expected outputs