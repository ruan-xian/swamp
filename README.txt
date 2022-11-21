0=|				SWAMP README				|=0
		Ellen Guo (ekg2134) - System Architect
		Ken Miura (km3635) - Language Guru
		Alice Wang (aw3271) - Manager
		Ryan Xu (rx2189) - Tester
		Cindy Zhu (cwz2102) - Tester

1.	Our file structure consists of three main files:
		scanner.mll
		parser.mly
		ast.mli			(ALICE CHANGE THIS TO AST.ML ONCE YOU DO PRINTING)
	In addition, we have a Makefile and some testing files:
		test_cases/		(folder containing test files)
		run_tests.sh	(bash script to run all tests)
		calc.ml
	In particular, calc.ml is of interest; it takes the parsed output from parser
	and generates behavior. Not all language features are implemented here, but
	conditional expressions and integer math are implemented (hence the name). Let
	expressions are also supported. This is basically a stopgap so we can test our 
	parsing before we do IR generation.

	Finally, some folders containing reference code are in our directory.
	(REMOVE THIS IF IT ISN'T IN OUR SUBMISSION ZIP)
		microc/
		hw2Q1/

2.	To run our tests, you can type "make test" from the base directory. This makes
	all the files and runs the run_tests.sh script. The script will display expected
	output as well as actual output (which may be an error).

	ALICE ONCE YOU ADD PRINTING ALSO DETAIL HOW WE CAN PRINT THE LEXED AND PARSED OUTPUT 

3.	Currently, we support the following features:
		int, float, char, and string types and literals
		variable assignment
		conditional expressions
		binary operations

4.	We are aiming to add the following features:
		lists
		functions and function definitions

5.	Team member contributions:
	Ryan Xu (rx2189):
		Lexing (except keywords)
		Parser foundations and grammar rework (Ellen you should put grammar rework in your section too)
		Testing framework (calc.ml, run_tests.sh, test cases)
		Variable scoping
	Ken Miura (km3635):
		Keyword lexing
		Refined reworked grammar to remove redundancies 
		Fixed parser and testing framework to work with updated grammar
