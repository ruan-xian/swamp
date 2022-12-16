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

	It also gives (vague) warnings when something semantically bad is done, so this also
	functions as a semantic checker in some ways (though it doesn't do semantic checking
	for lists and functions yet).

	Finally, some folders containing reference code are in our directory. These can be ignored.
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

	To run semantic checking on the test programs, you can type "make semant_test" from the base directory. 
	This makes swamp.native and runs the run_semant_tests.sh script. The script will display the name of 
	the program and either the SAST or a Fatal error if the program cannot be parsed or it failed semantic 
	checking. 

3.	Currently, we support the following features:
		int, float, char, and string types and literals
		variable assignment
		conditional expressions
		binary operations
		function definition (including syntactic sugar version)
		function application (including parenthesized application)

4.	We are aiming to add the following features:
		semantic checking for lists and functions (no time)

5.	Team member contributions:
	Ryan Xu (rx2189):
		Lexing (except keywords)
		Parser foundations + operators (arithmetic and boolean)
		Testing framework (calc.ml, run_tests.sh, test cases)
		Variable scoping
		Turning calc.ml into a weird semantic checker
	Ken Miura (km3635):
		Keyword and type lexing
		Refined reworked grammar to remove redundancies, updated LRM 
		Fixed parser and testing framework to work with updated grammar
		Implemented Lists and related operators (parser/scanner/print/test)
		Implemented List Comprehension (parser/scanner/print/test)
	Alice Wang (aw3271):
		Assigned task: Print out AST structure
		Added printing support to ast.ml 
		Automated testing for parse printing (run_print_tests.sh, print_test.ml, revised Makefile)
	Ellen Guo (ekg2134):
		Parser foundations + operators (if/then/else, worked with Ryan on arithmetic)
		Reworked grammar from initial LRM 
		Function definition and application + tests
	Cindy Zhu (cwz2102):
		Variable Types - parsing, printing, test cases
		Boolean and Unary Operators - parsing, scanning, printing, test cases
		Support for all variable types in calc.ml
		
	Team member contributions for final submission:
	Ryan Xu (rx2189):
		SAST
		Semant framework
		Semant support for
			Int, float, bool types and binary operators
			Assignment
			Return type inference for recursive functions
			Conditional expressions
	Alice Wang (aw3271): 
		SAST + semant support for String and Char literals 
		String and Char concatentation (semant.ml)
		Automated testing for semant checking (run_semant_tests.sh, revised Makefile)
		IR Gen for non-arithmetic Infix Op's and Unary Ops
