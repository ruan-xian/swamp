0=|				SWAMP README				|=0
		Ellen Guo (ekg2134) - System Architect
		Ken Miura (km3635) - Language Guru
		Alice Wang (aw3271) - Manager
		Ryan Xu (rx2189) - Tester
		Cindy Zhu (cwz2102) - Tester

1.	Our file structure consists of three main files:
		scanner.mll
		parser.mly
		ast.ml
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

2.	To run our calc tests, you can type "make test" from the base directory. This makes
	all the files and runs the run_tests.sh script. The script will display expected
	output as well as actual output (which may be an error).

	Here is some sample output; depending on your bash interpreter it may or may not look different.
	
> sh run_tests.sh
bad_assignment.swamp
        0=| Expected value: not_found |=0
        Fatal error: exception Not_found
bad_lex.swamp
        0=| Expected value: Illegal character ^ |=0
        Fatal error: exception Failure("illegal character ^")
bad_parse_assignment.swamp
        0=| Expected value: Parse_error |=0
        Fatal error: exception Stdlib.Parsing.Parse_error
bad_parse_conditional_no_else.swamp
        0=| Expected value: Parse_error |=0
        Fatal error: exception Stdlib.Parsing.Parse_error
conditional.swamp
        0=| Expected value: 18216 |=0
        18216

	To print out the parsed programs, you can type "make print_test" from the base directory. 
	This makes print_test.native and runs the run_print_tests.sh script. The script will display 
	the name of the program, the program itself, and either the parsed program or a fatal_error if 
	the program cannot be parsed. 

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
		Parser foundations + operators (arithmetic and boolean)
		Testing framework (calc.ml, run_tests.sh, test cases)
		Variable scoping
	Ken Miura (km3635):
		Keyword and type lexing
		Refined reworked grammar to remove redundancies 
		Fixed parser and testing framework to work with updated grammar
		Implemented Lists and related operators
	Alice Wang (aw3271):
		Assigned task: Print out AST structure
		Added printing support to ast.ml 
		Automated testing for parse printing (run_print_tests.sh, revised Makefile)
	Cindy Zhu (cwz2102):
		Variable Types
		Boolean and Unary Operators - parsing, scanning, and printing
		Support for all Variable types in calc.ml
		
