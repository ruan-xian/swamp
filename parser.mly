%{ open Ast %}

%token COMMA SEMI COLON LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE EOF
%token <int> INTLIT
%token <float> FLOATLIT
%token <string> ID STRINGLIT
%token <char> CHARLIT

// %left SEMICOLON
// %right ASSIGN
// %left PLUS MINUS
// %left TIMES DIVIDE

%start expr
%type <int> expr

// %%
%%

expr:
  COMMA { 0 }
//   expr PLUS   expr { Binop($1, Add, $3) }
// | expr MINUS  expr { Binop($1, Sub, $3) }
// | expr TIMES  expr { Binop($1, Mul, $3) }
// | expr DIVIDE expr { Binop($1, Div, $3) }
// | expr SEMICOLON expr { Seq($1, $3) }
// | ID ASSIGN expr { Asn($1, $3) }
// | ID { Var($1) }
// | LITERAL          { Lit($1) }
