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
    cinfixexp { InfixExp $1 }
  | dinfixexp { InfixExp $1 }
  | IF expr THEN expr ELSE expr { CondExp($2, $4, $6) }
  | LET ID ASSIGN expr IN expr { Assign($2, $4, $6) }
  
aexp:
    INTLIT { IntLit $1 }
  | FLOATLIT { FloatLit $1 }
  | STRINGLIT { StringLit $1 }
  | CHARLIT { CharLit $1 }
  | ID { Var $1 }
  | LPAREN expr RPAREN { ParenExp $2 }

cinfixexp: 
  | cinfixexp PLUS cinfixexp { InfixOp($1, Add, $3) }
  | cinfixexp MINUS cinfixexp { InfixOp($1, Sub, $3) }
  | cinfixexp MULT cinfixexp { InfixOp($1, Mul, $3) }
  | cinfixexp DIV cinfixexp { InfixOp($1, Div, $3) }
  | cinfixexp MOD cinfixexp { InfixOp($1, Mod, $3) }
  | cinfixexp EQUAL cinfixexp { InfixOp($1, Eq, $3) }
  | cinfixexp GREATER cinfixexp { InfixOp($1, Greater, $3) }
  | cinfixexp GEQ cinfixexp { InfixOp($1, Geq, $3) }
  | cinfixexp LESS cinfixexp { InfixOp($1, Less, $3) }
  | cinfixexp LEQ cinfixexp { InfixOp($1, Leq, $3) }
  | cinfixexp NEQ cinfixexp { InfixOp($1, Neq, $3) }
  | aexp { ArgExp $1 }

dinfixexp: 
  | cinfixexp PLUS dinfixexp { InfixOp($1, Add, $3) }
  | cinfixexp MINUS dinfixexp { InfixOp($1, Sub, $3) }
  | cinfixexp MULT dinfixexp { InfixOp($1, Mul, $3) }
  | cinfixexp DIV dinfixexp { InfixOp($1, Div, $3) }
  | cinfixexp MOD dinfixexp { InfixOp($1, Mod, $3) }
  | cinfixexp EQUAL dinfixexp { InfixOp($1, Eq, $3) }
  | cinfixexp GREATER dinfixexp { InfixOp($1, Greater, $3) }
  | cinfixexp GEQ dinfixexp { InfixOp($1, Geq, $3) }
  | cinfixexp LESS dinfixexp { InfixOp($1, Less, $3) }
  | cinfixexp LEQ dinfixexp { InfixOp($1, Leq, $3) }
  | cinfixexp NEQ dinfixexp { InfixOp($1, Neq, $3) }
  
