%{ open Ast %}

%token COMMA SEMI COLON LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE EOF
%token PLUS MINUS MULT DIV MOD ASSIGN EQUAL LESS GREATER LEQ GEQ NEQ
%token IN LET IF THEN ELSE

%token <int> INTLIT
%token <float> FLOATLIT
%token <string> ID STRINGLIT
%token <char> CHARLIT

// %left SEMICOLON
%left IN
%right ASSIGN
%left PLUS MINUS
%left MULT DIV MOD

%start expr
%type <Ast.expr> expr

// %%
%%

expr:
    cinfixexp { InfixExp $1 }
  | dinfixexp { InfixExp $1 }
  | IF expr THEN expr ELSE expr { CondExp($2, $4, $6) }
  
aexp:
    INTLIT { IntLit $1 }
  | ID { Var $1 }
  | LPAREN aexp RPAREN { $2 }
  | LET ID ASSIGN aexp IN aexp { Assign($2, $4, $6) }
  

cinfixexp: 
  | cinfixexp PLUS cinfixexp { InfixOp($1, Add, $3) }
  | cinfixexp MINUS cinfixexp { InfixOp($1, Sub, $3) }
  | cinfixexp MULT cinfixexp { InfixOp($1, Mul, $3) }
  | cinfixexp DIV cinfixexp { InfixOp($1, Div, $3) }
  | cinfixexp MOD cinfixexp { InfixOp($1, Mod, $3) }
  | aexp  { ArgExp $1 }

dinfixexp: 
  | cinfixexp PLUS dinfixexp { InfixOp($1, Add, $3) }
  | cinfixexp MINUS dinfixexp { InfixOp($1, Sub, $3) }
  | cinfixexp MULT dinfixexp { InfixOp($1, Mul, $3) }
  | cinfixexp DIV dinfixexp { InfixOp($1, Div, $3) }
  | cinfixexp MOD dinfixexp { InfixOp($1, Mod, $3) }
  
