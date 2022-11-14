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
  infixexp { Infix $1 }

infixexp:
    lexp PLUS infixexp { InfixOp($1, Add, $3) }
  | lexp MINUS infixexp { InfixOp($1, Sub, $3) }
  | lexp MULT infixexp { InfixOp($1, Mul, $3) }
  | lexp DIV infixexp { InfixOp($1, Div, $3) }
  | lexp MOD infixexp { InfixOp($1, Mod, $3) }
  | lexp { LetExpr1 $1 }
// (* TODO: Unary Minus *)

lexp:
    LET decls IN expr { LetExpr2($2, $4)}
  | fexp { FunctionApp $1 }

fexp:
    aexp { ArgExp $1 }
  
aexp:
    INTLIT { IntLit $1 }
  | ID { Var $1 }
  | LPAREN expr RPAREN { ParenExp $2}

decls:
    ID ASSIGN expr { Assign($1, $3)}
