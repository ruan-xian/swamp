%{ open Ast %}

%token COMMA SEMI COLON LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE EOF
%token PLUS MINUS MULT DIV MOD ASSIGN EQUAL LESS GREATER LEQ GEQ NEQ
%token IN LET IF THEN ELSE WHERE FOR BY OVER ONION STRICT FUN
%token NONE WILDCARD
%token AND OR NOT
%token TYPE INTTYPE FLOATTYPE CHARTYPE STRTYPE BOOLTYPE

%token <int> INTLIT
%token <bool> BOOLLIT
%token <float> FLOATLIT
%token <string> ID STRINGLIT
%token <char> CHARLIT

// %left SEMICOLON
%left IN
%left OR AND NOT
%left EQUAL GREATER LESS LEQ GEQ NEQ
%left ELSE

%right ASSIGN
%left PLUS MINUS
%left MULT DIV MOD

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
  | expr NOT expr { InfixOp($1, Not, $3) }

    // Literals
  | INTLIT { IntLit $1 }
  | BOOLLIT { BoolLit $1 }
  | FLOATLIT { FloatLit $1 }
  | STRINGLIT { StringLit $1 }
  | CHARLIT { CharLit $1 }

    // Parenthesized Expressions
  | LPAREN expr RPAREN { $2 }
