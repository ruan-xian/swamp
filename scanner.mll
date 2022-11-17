{ open Parser }

let letter = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']
let symbol = [' '-'!' '#'-'&' '('-'[' ']'-'~'] 
    (* excludes single/double quote, tab, backslash, and DEL *)
let id = letter (letter | digit)*

let decimal = digit+
let int = decimal
let exponent = ['e' 'E']['+' '-']?decimal
let float = (((decimal?'.'decimal) | (decimal '.' decimal?))exponent?) | (decimal exponent)

let escape = '\\' ['\\' '\'' '"' 'n' 't']
let any = letter | digit | symbol
let stringchar = any | escape
let char = "'" (stringchar | '"') "'"
let string = '"' (stringchar | "'")* '"'

let opencomment = "0=|"
let comment = opencomment (any | ['\t' '\\' '"' '\''])* "\n"
let whitechar = [' ' '\t'] | ('\r'?'\n')
let whitestuff = whitechar | comment
let whitespace = whitestuff+

rule tokenize = parse
    whitespace {tokenize lexbuf}
(* special characters *)
  | ',' { COMMA }
  | ';' { SEMI }
  | ':' { COLON }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '{' { LBRACE }
  | '}' { RBRACE }
(* operators *)
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MULT }
  | '/' { DIV }
  | '%' { MOD }
  | "==" { EQUAL }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | "<" { LESS }
  | ">" { GREATER }
  | "!=" { NEQ }
  | '=' { ASSIGN }
(* keywords *)
  | "in" { IN }
  | "let" { LET }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
(* non reserved *)
  | int as lexeme { INTLIT(int_of_string lexeme) }
  | float as lexeme { FLOATLIT(float_of_string lexeme) }  
  | char as lexeme { CHARLIT(String.get lexeme 1) }
  | string as lexeme { STRINGLIT(lexeme) }
  | id as lexeme { ID(lexeme) }
  | eof { EOF }