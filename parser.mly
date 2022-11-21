%{ open Ast %}

%token COMMA SEMI COLON LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE EOF
%token PLUS MINUS MULT DIV MOD ASSIGN EQUAL LESS GREATER LEQ GEQ NEQ
%token IN LET IF THEN ELSE WHERE FOR BY OVER ONION STRICT FUN
%token NONE WILDCARD
%token TLIT FLIT AND OR NOT
%token TYPE INTTYPE FLOATTYPE CHARTYPE STRTYPE BOOLTYPE

%token <int> INTLIT
%token <float> FLOATLIT
%token <string> ID STRINGLIT
%token <char> CHARLIT

// %left SEMICOLON
%left IN
%right ASSIGN
%left OR AND NOT
%left EQUAL GREATER LESS LEQ GEQ NEQ
%left PLUS MINUS
%left MULT DIV MOD

%start program
%type <Ast.program> program

// %%
%%

program:
    expr EOF { Expr $1 }

expr:
    lexp PLUS expr { InfixOp($1, Add, $3) }
  | lexp MINUS expr { InfixOp($1, Sub, $3) }
  | lexp MULT expr { InfixOp($1, Mul, $3) }
  | lexp DIV expr { InfixOp($1, Div, $3) }
  | lexp MOD expr { InfixOp($1, Mod, $3) }
  | lexp EQUAL expr { InfixOp($1, Eq, $3) }
  | lexp GREATER expr { InfixOp($1, Greater, $3) }
  | lexp GEQ expr { InfixOp($1, Geq, $3) }
  | lexp LESS expr { InfixOp($1, Less, $3) }
  | lexp LEQ expr { InfixOp($1, Leq, $3) }
  | lexp NEQ expr { InfixOp($1, Neq, $3) }
  | lexp { LeftExp $1 }

lexp:
    IF expr THEN expr ELSE expr { CondExp($2, $4, $6) }
  | LET ID ASSIGN expr IN expr { Assign($2, $4, $6) }
  | aexp { ArgExp $1 }
  
aexp:
    INTLIT { IntLit $1 }
  | ID { Var $1 }
  | LPAREN expr RPAREN { ParenExp $2 }
