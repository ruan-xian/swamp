%{ open Ast %}

%token COMMA SEMI LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE EOF
%token CONS CAT HEAD TAIL
%token PLUS MINUS MULT DIV MOD ASSIGN EQUAL LESS GREATER LEQ GEQ NEQ ARROW
%token IN LET IF THEN ELSE WHERE FOR BY OVER ONION STRICT FUN
%token NONE WILDCARD
%token AND OR NOT UMINUS
%token TYPE INTTYPE FLOATTYPE CHARTYPE STRTYPE BOOLTYPE

%token <int> INTLIT
%token <bool> BOOLLIT
%token <float> FLOATLIT
%token <string> ID STRINGLIT
%token <char> CHARLIT

// %left SEMI
%left IN
%right ARROW
%left OR AND
%left EQUAL GREATER LESS LEQ GEQ NEQ
%left ELSE

%right ASSIGN
%left PLUS MINUS
%left MULT DIV MOD
%nonassoc UMINUS NOT

%left CONS CAT
%nonassoc TAIL HEAD

%start program
%type <Ast.program> program

// %%
%%

program:
    expr EOF { Expr $1 }

expr:
    // Conditional
    IF expr THEN expr ELSE expr { CondExp($2, $4, $6) }
      
    // Let Expression
  | LET ID ASSIGN expr IN expr { Assign($2, $4, $6) }

    // Function Definition
  | FUN LPAREN formals_opt RPAREN ARROW expr { FunExp($3, $6) }
    // Syntactic Sugar Function Def
  | LET ID LPAREN formals_opt RPAREN ASSIGN expr IN expr { FunAssign($2, $4, $7, $9) }
    // Function Application
  | ID LPAREN args_opt RPAREN { FunApp($1, $3) }
  | LPAREN expr RPAREN LPAREN args_opt RPAREN { FunExpApp($2, $5) }

    // Variable
  | ID { Var $1 }

    // Arithmetic Operations
  | expr PLUS expr { InfixOp($1, Add, $3) }
  | expr MINUS expr { InfixOp($1, Sub, $3) }
  | expr MULT expr { InfixOp($1, Mul, $3) }
  | expr DIV expr { InfixOp($1, Div, $3) }
  | expr MOD expr { InfixOp($1, Mod, $3) }

    // Boolean Operations
  | expr EQUAL expr { InfixOp($1, Eq, $3) }
  | expr GREATER expr { InfixOp($1, Greater, $3) }
  | expr GEQ expr { InfixOp($1, Geq, $3) }
  | expr LESS expr { InfixOp($1, Less, $3) }
  | expr LEQ expr { InfixOp($1, Leq, $3) }
  | expr NEQ expr { InfixOp($1, Neq, $3) }
  | expr AND expr { InfixOp($1, And, $3) }
  | expr OR expr { InfixOp($1, Or, $3) }

    // Urnary Operations
  | NOT expr { UnaryOp(Not, $2) }
  | MINUS expr %prec UMINUS { UnaryOp(UMinus, $2) }

    // List Operations
  | expr CONS expr { InfixOp($1, Cons, $3) }
  | expr CAT expr { InfixOp($1, Cat, $3) }
  | HEAD expr { PrefixOp(Head, $2) }
  | TAIL expr { PrefixOp(Tail, $2) }

    // Literals
  | INTLIT { IntLit $1 }
  | BOOLLIT { BoolLit $1 }
  | FLOATLIT { FloatLit $1 }
  | STRINGLIT { StringLit $1 }
  | CHARLIT { CharLit $1 }

    // Parenthesized Expressions
  | LPAREN expr RPAREN { ParenExp($2) }

    // Lists
  | LBRACKET iter RBRACKET { ListExp($2) }

iter:
    expr { [$1] }
  | expr SEMI iter { $1 :: $3 }
  |     { [] }


formals_opt:
    /*nothing*/ { [] }
  | formals_list { $1 } 

formals_list:
  | ID { [$1] }
  | ID COMMA formals_list { $1::$3 }

args_opt:
    /*nothing*/ { [] }
  | args_list { $1 } 

args_list:
  | expr { [$1] }
  | expr COMMA args_list { $1::$3 }
