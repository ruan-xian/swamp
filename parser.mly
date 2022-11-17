%{ open Ast %}

%token COMMA SEMI COLON LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE EOF
%token PLUS MINUS MULT DIV MOD ASSIGN EQUAL LESS GREATER LEQ GEQ NEQ
%token IN LET

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
    aexp { ArgExp $1 }
  
aexp:
    INTLIT { IntLit $1 }
  | ID { Var $1 }
  | LPAREN aexp RPAREN { $2 }
  | LET ID ASSIGN aexp IN aexp { Assign($2, $4, $6) }
  | aexp PLUS aexp { InfixOp($1, Add, $3) }
  | aexp MINUS aexp { InfixOp($1, Sub, $3) }
  | aexp MULT aexp { InfixOp($1, Mul, $3) }
  | aexp DIV aexp { InfixOp($1, Div, $3) }
  | aexp MOD aexp { InfixOp($1, Mod, $3) }
