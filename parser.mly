%{ open Ast %}

%token COMMA SEMI LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE EOF
%token CONS CAT HEAD TAIL
%token PLUS MINUS MULT DIV MOD ASSIGN EQUAL LESS GREATER LEQ GEQ NEQ
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
%left FOR IN IF
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
  | HEAD expr { UnaryOp(Head, $2) }
  | TAIL expr { UnaryOp(Tail, $2) }

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
  | LBRACKET comp RBRACKET { $2 }

iter:
    expr { [$1] }
  | expr SEMI iter { $1 :: $3 }
  |     { [] }

comp:
    expr FOR ID IN expr qual { ListComp($1, CompFor($3, $5) :: $6) }
  | expr FOR ID IN ID qual { ListComp($1, CompFor($3, Var($5)) :: $6) }

qual:
    FOR ID IN expr qual { CompFor($2, $4) :: $5 }
  | FOR ID IN ID qual { CompFor($2, Var($4)) ::  $5 }
  | IF expr qual { CompIf($2) :: $3 }
  |      { [] }


