{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
}

(* Regular expressions *)
let whitespace = [' ' '\t' '\n' '\r']+
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let digit = ['0'-'9']
let identifier = (lowercase | uppercase | '_') (lowercase | uppercase | digit | '_')*
let variable = uppercase (lowercase | uppercase | digit | '_')*
let constant = lowercase (lowercase | uppercase | digit | '_')*

rule token = parse
  | whitespace              { token lexbuf }     (* Skip whitespace *)
  | "(*"                    { comment lexbuf }   (* Handle comments *)
  | "%"                     { line_comment lexbuf }  (* Line comments like Prolog *)

  (* Keywords and operators *)
  | "Q="                    { QUERY }
  | ":-"                    { IMPLIES_RULE }
  | "Not"                   { NOT }
  | "And"                   { AND }
  | "Or"                    { OR }
  | "Implies"               { IMPLIES }
  | "Iff"                   { IFF }
  | "T"                     { TRUE }
  | "F"                     { FALSE }

  (* Delimiters *)
  | '('                     { LPAREN }
  | ')'                     { RPAREN }
  | '['                     { LBRACK }
  | ']'                     { RBRACK }
  | ','                     { COMMA }
  | '.'                     { DOT }

  (* Identifiers *)
  | variable as v           { VARIABLE v }
  | constant as c           { CONSTANT c }

  | eof                     { EOF }
  | _ as c                  { failwith (Printf.sprintf "Unexpected character: %c" c) }

and comment = parse
  | "*)"                    { token lexbuf }
  | _                       { comment lexbuf }
  | eof                     { failwith "Unterminated comment" }

and line_comment = parse
  | '\n'                    { token lexbuf }
  | _                       { line_comment lexbuf }
  | eof                     { EOF }
