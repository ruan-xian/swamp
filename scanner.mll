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
let string = ('"' (stringchar | "'")* '"') |  ("'" (stringchar | '"')* "'")

let opencomment = "0=|"
let comment = opencomment (any | ['\t' '\\' '"' '\''])* ("\n" | eof)
let whitechar = [' ' '\t'] | ('\r'?'\n')
let whitestuff = whitechar | comment
let whitespace = whitestuff+

rule tokenize = parse
    whitespace {tokenize lexbuf}
(* special characters *)
  | ',' { COMMA }
  | ';' { SEMI }
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
(* list operators *)
  | "@" { CAT }
  | "::" { CONS }
  | "head" { HEAD }
  | "tail" { TAIL }
  | "isEmpty" { ISEMPTY }
(* function operators *)
  | "->" { ARROW } 
(* boolean operators *)
  | "and" { AND }
  | "or" { OR }
  | "not" { NOT }
(* keywords *)
  | "in" { IN }
  | "let" { LET }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "where" { WHERE }
  | "for" { FOR }
  | "by" { BY }
  | "type" { TYPE }
  | "over" { OVER }
  | "onion" { ONION }
  | "strict" { STRICT }
  | "fun" { FUN }
  | "None" { NONE }
  | "_" { WILDCARD }
(* boolean literals *)
  | "True" { BOOLLIT(true) }
  | "False" { BOOLLIT(false) }
(* types *)
  | "int" { INTTYPE }
  | "float" { FLOATTYPE }
  | "string" { STRTYPE }
  | "bool" { BOOLTYPE }
  | "list" { LISTTYPE }
  | ":" { COLON } 
(* non reserved *)
  | int as lexeme { INTLIT(int_of_string lexeme) }
  | float as lexeme { FLOATLIT(float_of_string lexeme) }  
  | string as lexeme { STRINGLIT(Scanf.unescaped (String.sub lexeme 1 ((String.length lexeme) - 2))) }
  (* String.sub  *)
  | id as lexeme { ID(lexeme) }
  | eof { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }
