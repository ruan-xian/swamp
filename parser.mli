type token =
  | COMMA
  | SEMI
  | COLON
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | EOF
  | PLUS
  | MINUS
  | MULT
  | DIV
  | MOD
  | ASSIGN
  | EQUAL
  | LESS
  | GREATER
  | LEQ
  | GEQ
  | NEQ
  | IN
  | LET
  | INTLIT of (int)
  | FLOATLIT of (float)
  | ID of (string)
  | STRINGLIT of (string)
  | CHARLIT of (char)

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
