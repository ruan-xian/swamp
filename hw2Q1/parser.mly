%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE EOF SEMICOLON ASSIGN
%token <int> LITERAL
%token <string> ID

%left SEMICOLON
%right ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE

%start expr
%type <Ast.expr> expr

%%

expr:
  expr PLUS   expr { Binop($1, Add, $3) }
| expr MINUS  expr { Binop($1, Sub, $3) }
| expr TIMES  expr { Binop($1, Mul, $3) }
| expr DIVIDE expr { Binop($1, Div, $3) }
| expr SEMICOLON expr { Seq($1, $3) }
| ID ASSIGN expr { Asn($1, $3) }
| ID { Var($1) }
| LITERAL          { Lit($1) }
