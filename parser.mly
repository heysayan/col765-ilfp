%{
open List

(* Import types from the resolution engine *)
type symbol = string * int

type tree =
  | V of string
  | C of {node: symbol; children: tree list}

type formula =
  | T
  | F
  | Pred of {predicate: symbol; members: tree list}
  | Not of formula
  | And of formula * formula
  | Or of formula * formula
  | Implies of formula * formula
  | Iff of formula * formula

type hornClause =
  | Fact of formula
  | Rule of formula * (formula list)
  | Goal of (formula list)

type program = hornClause list

%}

/* Token declarations */
%token <string> VARIABLE
%token <string> CONSTANT
%token LPAREN RPAREN LBRACK RBRACK
%token COMMA DOT
%token QUERY               /* Q= */
%token IMPLIES_RULE        /* :- */
%token NOT AND OR IMPLIES IFF
%token TRUE FALSE
%token EOF

/* Precedence and associativity */
%right IFF
%right IMPLIES
%left OR
%left AND
%nonassoc NOT

/* Start symbol */
%start program
%type <program> program

%%

/* Program is a list of horn clauses */
program:
  | clause_list EOF { List.rev $1 }
;

clause_list:
  | /* empty */                { [] }
  | clause_list horn_clause    { $2 :: $1 }
;

horn_clause:
  | QUERY formula_list DOT     { Goal (List.rev $2) }
  | formula DOT                { Fact $1 }
  | formula IMPLIES_RULE formula_list DOT
                               { Rule ($1, List.rev $3) }
;

formula_list:
  | formula                    { [$1] }
  | formula_list COMMA formula { $3 :: $1 }
;

/* Formulas with logical operators */
formula:
  | TRUE                              { T }
  | FALSE                             { F }
  | NOT formula                       { Not $2 }
  | formula AND formula               { And ($1, $3) }
  | formula OR formula                { Or ($1, $3) }
  | formula IMPLIES formula           { Implies ($1, $3) }
  | formula IFF formula               { Iff ($1, $3) }
  | LPAREN formula RPAREN             { $2 }
  | predicate                         { $1 }
;

/* Predicate: identifier(term, term, ...) or identifier */
predicate:
  | CONSTANT LPAREN term_list RPAREN
    {
      let terms = List.rev $3 in
      Pred {predicate = ($1, List.length terms); members = terms}
    }
  | CONSTANT
    {
      Pred {predicate = ($1, 0); members = []}
    }
;

term_list:
  | term                       { [$1] }
  | term_list COMMA term       { $3 :: $1 }
;

/* Terms: variables or compound terms (constants with arguments) */
term:
  | VARIABLE                             { V $1 }
  | CONSTANT                             { C {node = ($1, 0); children = []} }
  | CONSTANT LPAREN term_list RPAREN
    {
      let children = List.rev $3 in
      C {node = ($1, List.length children); children = children}
    }
;

%%
