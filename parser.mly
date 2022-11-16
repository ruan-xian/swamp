%{ open Ast %}

%token COMMA SEMI COLON LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE EOF
%token PLUS MINUS MULT DIV MOD ASSIGN EQUAL LESS GREATER LEQ GEQ NEQ
%token IN LET

%token <int> INTLIT
%token <float> FLOATLIT
%token <string> ID STRINGLIT
%token <char> CHARLIT

// %left SEMICOLON
%right ASSIGN
%left PLUS MINUS
%left MULT DIV MOD

%start expr
%type <Ast.expr> expr

// %%
%%

expr:
  cinfixexp { Infix $1 }
  | dinfixexp { Infix $1 }

cinfixexp:
    cinfixexp PLUS cinfixexp { InfixOp($1, Add, $3) }
  | cinfixexp MINUS cinfixexp { InfixOp($1, Sub, $3) }
  | cinfixexp MULT cinfixexp { InfixOp($1, Mul, $3) }
  | cinfixexp DIV cinfixexp { InfixOp($1, Div, $3) }
  | cinfixexp MOD cinfixexp { InfixOp($1, Mod, $3) }
  | fexp { FunctionApp $1 }
// (* TODO: Unary Minus *)

dinfixexp: 
    cinfixexp PLUS dinfixexp { InfixOp($1, Add, $3) }
  | cinfixexp MINUS dinfixexp { InfixOp($1, Sub, $3) }
  | cinfixexp MULT dinfixexp { InfixOp($1, Mul, $3) }
  | cinfixexp DIV dinfixexp { InfixOp($1, Div, $3) }
  | cinfixexp MOD dinfixexp { InfixOp($1, Mod, $3) }
  | LET decls IN expr { LetExpr2($2, $4)}

fexp:
    aexp { ArgExp $1 }
  
aexp:
    INTLIT { IntLit $1 }
  | ID { Var $1 }
  | LPAREN expr RPAREN { ParenExp $2 }

decls:
    ID ASSIGN expr { Assign($1, $3) }
