# Prolog-like Resolution Engine with Parser Frontend

## Table of Contents
1. [Overview](#overview)
2. [Components](#components)
3. [Resolution Engine (assgn7.ml)](#resolution-engine-assgn7ml)
4. [Lambda Calculus Interpreter (assgn8.ml)](#lambda-calculus-interpreter-assgn8ml)
5. [Frontend Components](#frontend-components)
6. [Building the Project](#building-the-project)
7. [Usage](#usage)
8. [Examples](#examples)
9. [Language Syntax](#language-syntax)
10. [Implementation Details](#implementation-details)

---

## Overview

This project implements a **Prolog-like resolution engine** for First-Order Logic (FOL) with a complete frontend parser built using **Ocamllex** and **Ocamlyacc**. The system can parse Prolog-style programs containing facts, rules, and queries, then use SLD-resolution with unification to find solutions.

### Key Features
- **Unification Algorithm**: Most General Unifier (MGU) for term matching
- **SLD Resolution**: Backward chaining resolution for Horn clauses
- **Parser Frontend**: Professional lexer/parser using Ocamllex and Ocamlyacc
- **Interactive REPL**: Read-Eval-Print Loop for interactive queries
- **File-based Execution**: Load and execute programs from files

---

## Components

The project consists of the following files:

| File | Purpose |
|------|---------|
| `assgn7.ml` | Core resolution engine with unification and resolution algorithms |
| `assgn8.ml` | Lambda calculus interpreter with Krivine machine (separate module) |
| `lexer.mll` | Lexical analyzer specification (Ocamllex) |
| `parser.mly` | Grammar specification (Ocamlyacc) |
| `assgn9.ml` | Main interpreter integrating lexer, parser, and resolution engine |
| `Makefile` | Build configuration |
| `test_program.pl` | Example Prolog-like program for testing |

---

## Resolution Engine (assgn7.ml)

### Overview
The resolution engine implements a complete First-Order Logic theorem prover using **Horn clause resolution** with **unification**.

### Core Data Types

#### 1. Terms (`tree`)
```ocaml
type symbol = string * int  (* name, arity *)

type tree =
  | V of string                              (* Variable *)
  | C of {node: symbol; children: tree list} (* Compound term/Constant *)
```

**Examples:**
- Variable: `V "X"`
- Constant: `C {node = ("john", 0); children = []}`
- Compound: `C {node = ("f", 2); children = [V "X"; V "Y"]}`

#### 2. Formulas (`formula`)
```ocaml
type formula =
  | T                                        (* True *)
  | F                                        (* False *)
  | Pred of {predicate: symbol; members: tree list}
  | Not of formula
  | And of formula * formula
  | Or of formula * formula
  | Implies of formula * formula
  | Iff of formula * formula
```

**Examples:**
- Atomic: `Pred {predicate = ("father", 2); members = [C "ramesh"; V "X"]}`
- Complex: `And (Pred {...}, Pred {...})`

#### 3. Horn Clauses (`hornClause`)
```ocaml
type hornClause =
  | Fact of formula                          (* Head only *)
  | Rule of formula * (formula list)         (* Head :- Body *)
  | Goal of (formula list)                   (* Query *)
```

**Examples:**
- Fact: `father(ramesh, rohan).`
- Rule: `parent(X, Y) :- father(X, Y).`
- Goal: `Q= parent(X, rohan).`

#### 4. Programs
```ocaml
type program = hornClause list
```

### Key Algorithms

#### 1. Unification (MGU)
```ocaml
val mgu : tree -> tree -> substitution
```

Computes the Most General Unifier between two terms.

**Algorithm:**
- Variables unify with any term (occur check prevents infinite structures)
- Compound terms unify if they have the same functor and arity
- Recursively unify children and compose substitutions

**Example:**
```ocaml
mgu (V "X") (C {node = ("john", 0); children = []})
(* Returns: [("X", C {node = ("john", 0); children = []})] *)
```

#### 2. Substitution Application
```ocaml
val subst : substitution -> tree -> tree
val formula_subst : substitution -> formula -> formula
```

Applies a substitution to a term or formula, replacing variables with their bindings.

#### 3. Substitution Composition
```ocaml
val compose_subst : substitution -> substitution -> substitution
```

Composes two substitutions: `σ₁ ∘ σ₂`

#### 4. SLD Resolution
```ocaml
val resolver : program -> hornClause -> bool * (substitution list)
```

**Main resolution algorithm:**
1. Takes a program (facts and rules) and a goal
2. Uses backward chaining to resolve goals
3. Returns success/failure and all possible substitutions

**Resolution Strategy:**
- **Goal Selection**: Leftmost goal selected first
- **Clause Selection**: Sequential search through program
- **Search**: Depth-first with backtracking
- **Completeness**: Complete for Horn clauses

**Example Flow:**
```
Goal: parent(X, rohan)
Program:
  1. father(ramesh, rohan).
  2. parent(X, Y) :- father(X, Y).

Step 1: Unify goal with rule head: parent(X, rohan) ≈ parent(X', Y')
        σ = {X'/X, Y'/rohan}
Step 2: New goal: father(X, rohan)
Step 3: Unify with fact: father(X, rohan) ≈ father(ramesh, rohan)
        σ' = {X/ramesh}
Step 4: Success! Return σ ∘ σ' = {X/ramesh}
```

### Well-Formedness Checking
```ocaml
val check_program : program -> predicateSignature -> signature ->
                    bool * predicateSignature * signature
```

Ensures:
- All predicates used consistently with same arity
- All function symbols used consistently
- Terms are well-formed

### Helper Functions

| Function | Purpose |
|----------|---------|
| `vars` | Extract variables from a term |
| `wftree` | Check if a term is well-formed |
| `wfformula` | Check if a formula is well-formed |
| `formula_vars` | Extract variables from a formula |
| `formula_mgu` | Unification for formulas |
| `formula_equality` | Structural equality for formulas |

---

## Lambda Calculus Interpreter (assgn8.ml)

### Overview
A separate module implementing a **lambda calculus interpreter** using the **Krivine machine** for call-by-name evaluation.

### Core Data Types

#### Lambda Expressions
```ocaml
type lamexp =
  | V of string              (* Variable *)
  | App of lamexp * lamexp   (* Application *)
  | Lam of string * lamexp   (* Abstraction *)
```

#### Closures
```ocaml
type closure = Closure of lamexp * ((string * closure) list)
type gamma = (string * closure) list  (* Environment *)
```

### Key Features

#### 1. Substitution with Capture Avoidance
```ocaml
val subst : string -> lamexp -> lamexp -> lamexp
```

Implements capture-avoiding substitution using fresh variable generation.

#### 2. Krivine Machine
```ocaml
val krivine_machine : closure list -> closure -> closure list
```

**Abstract machine for call-by-name evaluation:**
- Uses a stack of closures
- Evaluates to weak head normal form (WHNF)
- Efficient lazy evaluation

**Machine states:**
- `(Closure(V x, γ) :: S)` → lookup x in γ
- `(Closure(Lam(x, e), γ) :: c :: S)` → substitute and continue
- `(Closure(App(e1, e2), γ) :: S)` → push e2, focus on e1

#### 3. Call-by-Name Evaluation
```ocaml
val cbn : lamexp -> lamexp
```

Evaluates a lambda expression to WHNF.

### Example Usage

#### Church Numerals
```ocaml
let zero = Lam("f", Lam("x", V "x"))
let one = Lam("f", Lam("x", App(V "f", V "x")))
let sum = (* \m.\n.\f.\x. m f (n f x) *)
```

#### Combinators
```ocaml
let k = Lam("x", Lam("y", V "x"))  (* K combinator *)
let s = (* \x.\y.\z. (x z)(y z) *)  (* S combinator *)
```

---

## Frontend Components

### Lexer (lexer.mll)

**Purpose:** Tokenizes the input string into meaningful tokens.

#### Token Types
```ocaml
VARIABLE      (* Uppercase identifiers: X, Y, Person *)
CONSTANT      (* Lowercase identifiers: john, mary, foo *)
LPAREN        (* ( *)
RPAREN        (* ) *)
LBRACK        (* [ *)
RBRACK        (* ] *)
COMMA         (* , *)
DOT           (* . *)
QUERY         (* Q= *)
IMPLIES_RULE  (* :- *)
NOT           (* Not *)
AND           (* And *)
OR            (* Or *)
IMPLIES       (* Implies *)
IFF           (* Iff *)
TRUE          (* T *)
FALSE         (* F *)
EOF           (* End of file *)
```

#### Special Features
- **Comments**:
  - Multi-line: `(* comment *)`
  - Single-line: `% comment`
- **Whitespace**: Automatically skipped
- **Case sensitivity**: Variables start with uppercase, constants with lowercase

#### Example Tokenization
```
Input:  father(ramesh, X).
Tokens: CONSTANT("father") LPAREN CONSTANT("ramesh") COMMA VARIABLE("X") RPAREN DOT
```

### Parser (parser.mly)

**Purpose:** Parses token stream into an Abstract Syntax Tree (AST).

#### Grammar Structure

##### 1. Program
```
program ::= clause_list EOF
clause_list ::= ε | clause_list horn_clause
```

##### 2. Horn Clauses
```
horn_clause ::= QUERY formula_list DOT            (Goal)
             |  formula DOT                       (Fact)
             |  formula IMPLIES_RULE formula_list DOT  (Rule)
```

##### 3. Formulas
```
formula ::= TRUE | FALSE
         |  NOT formula
         |  formula AND formula
         |  formula OR formula
         |  formula IMPLIES formula
         |  formula IFF formula
         |  LPAREN formula RPAREN
         |  predicate
```

**Precedence** (lowest to highest):
1. `IFF` (right associative)
2. `IMPLIES` (right associative)
3. `OR` (left associative)
4. `AND` (left associative)
5. `NOT` (non-associative)

##### 4. Predicates
```
predicate ::= CONSTANT LPAREN term_list RPAREN
           |  CONSTANT
```

##### 5. Terms
```
term ::= VARIABLE
      |  CONSTANT
      |  CONSTANT LPAREN term_list RPAREN
```

#### Error Handling
- Syntax errors report line and column numbers
- Graceful error messages for common mistakes

### Main Interpreter (assgn9.ml)

**Purpose:** Integrates all components and provides user interface.

#### Key Functions

##### 1. Parsing
```ocaml
val parse_string : string -> program
val parse_file : string -> program
```

Converts text input into AST using lexer and parser.

##### 2. Program Splitting
```ocaml
val split_program : program -> program * (hornClause list)
```

Separates facts/rules from queries.

##### 3. Query Execution
```ocaml
val run_query : program -> hornClause -> unit
```

Runs a single query and prints results.

##### 4. Pretty Printing
```ocaml
val tree_to_string : tree -> string
val formula_to_string : formula -> string
val hornclause_to_string : hornClause -> string
val substitution_to_string : substitution -> string
```

Converts internal representations back to readable format.

##### 5. REPL
```ocaml
val repl : unit -> unit
```

Interactive Read-Eval-Print Loop.

---

## Building the Project

### Prerequisites
- OCaml compiler (version 4.08 or later)
- Ocamllex (lexer generator)
- Ocamlyacc (parser generator)

### Build Commands

```bash
# Build bytecode executable
make bytecode

# Build native code executable (faster)
make native

# Clean generated files
make clean

# Show help
make help
```

### Build Process

1. **Generate Parser**: `ocamlyacc parser.mly` → `parser.ml`, `parser.mli`
2. **Generate Lexer**: `ocamllex lexer.mll` → `lexer.ml`
3. **Compile Parser**: `ocamlc -c parser.mli parser.ml`
4. **Compile Lexer**: `ocamlc -c lexer.ml`
5. **Compile Main**: `ocamlc -c assgn9.ml`
6. **Link**: `ocamlc -o prolog_engine parser.cmo lexer.cmo assgn9.cmo`

---

## Usage

### Interactive Mode (REPL)

```bash
./prolog_engine
```

**Example session:**
```
Prolog-like Resolution Engine REPL
Enter facts, rules, and queries. End each with a dot (.)
Type 'quit.' to exit.

?- father(john, mary).
Added 1 clause(s) to the program.
?- Q= father(john, X).
Query: Q= father(john, X).
Success!
Solutions:
  1. {X = mary}

?- quit.
Goodbye!
```

### File Mode

```bash
./prolog_engine test_program.pl
```

Loads program from file, executes all queries, and prints results.

### Example Output

```
Loading program from: test_program.pl

Program loaded successfully.
Facts and Rules:
  father(ramesh, rohan).
  mother(sita, rohan).
  wife(sita, ramesh).
  mother(X, Y) :- wife(X, Z), father(Z, Y).

Running 2 queries...

========== Query 1 ==========
Query: Q= father(ramesh, X).
Success!
Solutions:
  1. {X = rohan}

========== Query 2 ==========
Query: Q= father(ramesh, M), mother(N, rohan).
Success!
Solutions:
  1. {M = rohan, N = sita}
```

---

## Examples

### Example 1: Simple Facts and Queries

**Program:**
```prolog
father(ramesh, rohan).
mother(sita, rohan).

Q= father(ramesh, X).
```

**Output:**
```
Solutions:
  1. {X = rohan}
```

### Example 2: Rules with Multiple Solutions

**Program:**
```prolog
father(ramesh, rohan).
wife(sita, ramesh).
wife(babita, ramesh).

mother(X, Y) :- wife(X, Z), father(Z, Y).

Q= mother(N, rohan).
```

**Output:**
```
Solutions:
  1. {N = sita}
  2. {N = babita}
```

### Example 3: Complex Queries

**Program:**
```prolog
son(luv, ram).
son(kush, ram).
child(A, B) :- son(A, B).
parent(E, F) :- child(F, E).

Q= parent(M, kush).
```

**Output:**
```
Solutions:
  1. {M = ram}
```

### Example 4: Negation

**Program:**
```prolog
friend(hanuman, ram).
Not friend(ravan, ram).

Q= Not friend(V, ram).
```

**Output:**
```
Solutions:
  1. {V = ravan}
```

### Example 5: Compound Terms

**Program:**
```prolog
equals(pair(X, Y), pair(X, Y)).

Q= equals(pair(a, b), Z).
```

**Output:**
```
Solutions:
  1. {Z = pair(a, b)}
```

---

## Language Syntax

### Grammar (BNF)

```bnf
<program>     ::= <clause>*
<clause>      ::= <fact> | <rule> | <goal>

<fact>        ::= <formula> '.'
<rule>        ::= <formula> ':-' <formula_list> '.'
<goal>        ::= 'Q=' <formula_list> '.'

<formula_list> ::= <formula> (',' <formula>)*

<formula>     ::= 'T' | 'F'
               |  'Not' <formula>
               |  <formula> 'And' <formula>
               |  <formula> 'Or' <formula>
               |  <formula> 'Implies' <formula>
               |  <formula> 'Iff' <formula>
               |  '(' <formula> ')'
               |  <predicate>

<predicate>   ::= <constant> '(' <term_list> ')'
               |  <constant>

<term_list>   ::= <term> (',' <term>)*

<term>        ::= <variable>
               |  <constant>
               |  <constant> '(' <term_list> ')'

<variable>    ::= [A-Z][a-zA-Z0-9_]*
<constant>    ::= [a-z][a-zA-Z0-9_]*
```

### Operator Precedence

| Precedence | Operator | Associativity |
|------------|----------|---------------|
| 1 (lowest) | `Iff` | Right |
| 2 | `Implies` | Right |
| 3 | `Or` | Left |
| 4 | `And` | Left |
| 5 (highest) | `Not` | Non-associative |

### Lexical Conventions

#### Variables
- Start with uppercase letter
- Followed by letters, digits, or underscores
- Examples: `X`, `Person`, `First_Name`

#### Constants
- Start with lowercase letter
- Followed by letters, digits, or underscores
- Examples: `john`, `mary`, `ram_123`

#### Comments
- Multi-line: `(* This is a comment *)`
- Single-line: `% This is a comment`

#### Whitespace
- Spaces, tabs, newlines are ignored
- Can be used freely for formatting

---

## Implementation Details

### Unification Algorithm

**Occur Check:**
```ocaml
if mem x (vars t2) then raise NOT_UNIFIABLE
```

Prevents creation of infinite terms like `X = f(X)`.

**Compound Term Unification:**
```ocaml
fold_left (fun mgu_ (ct1, ct2) ->
  compose_subst mgu_ (mgu (subst mgu_ ct1) (subst mgu_ ct2))
) [] (combine r.children r_.children)
```

Unifies children left-to-right, composing substitutions.

### Resolution Algorithm

**Traverser Function:**
- Searches through program for clauses that unify with current goal
- Collects all possible resolutions (backtracking points)

**Resolver Function:**
- Processes goals left-to-right
- For each goal, tries all possible resolutions
- Applies substitutions to remaining goals
- Eliminates duplicate subgoals

**Backtracking:**
```ocaml
match resolver_in prog goals (compose_subst sub s1) final with
  | (true, subs) -> caller rest_res subs true  (* Continue with more solutions *)
  | _ -> caller rest_res final (resultb || false)  (* Backtrack *)
```

### Parser Implementation

**Shift-Reduce Parser:**
- Generated by Ocamlyacc
- LALR(1) parsing
- Handles ambiguity through precedence declarations

**Error Recovery:**
- Reports exact position of syntax errors
- Provides meaningful error messages

### Memory Management

**List Implementation:**
```ocaml
type 'a list_ = Empty | Append of ('a list_ * 'a)
```

Custom list type for efficient append-at-end operations during resolution.

**Substitution Representation:**
```ocaml
type substitution = (string * tree) list
```

Association list mapping variable names to terms.

---

## Limitations and Extensions

### Current Limitations
1. **No Universal/Existential Quantifiers**: Only Horn clauses supported
2. **Depth-First Search**: Can loop on infinite paths
3. **No Optimization**: Naive resolution strategy
4. **No Constraint Solving**: Pure symbolic unification

### Possible Extensions
1. **Iterative Deepening**: Prevent infinite loops
2. **Tabling/Memoization**: Cache intermediate results
3. **Better Search Strategies**: Heuristics for clause selection
4. **Constraint Logic Programming**: Add constraint domains
5. **Module System**: Organize large programs
6. **Debugging Tools**: Trace resolution steps
7. **Type System**: Static type checking for predicates

---

## Testing

### Test Program Structure

```prolog
% Facts
father(ramesh, rohan).
mother(sita, rohan).

% Rules
parent(X, Y) :- father(X, Y).
parent(X, Y) :- mother(X, Y).

% Queries
Q= parent(ramesh, rohan).
Q= parent(X, rohan).
```

### Running Tests

```bash
# Run test program
./prolog_engine test_program.pl

# Or use make target
make test
```

### Expected Output

Each query should produce:
- Success/Failure status
- All possible substitutions (solutions)
- Clear formatting

---

## References

### Unification
- Robinson, J. A. (1965). "A Machine-Oriented Logic Based on the Resolution Principle"
- Martelli, A., & Montanari, U. (1982). "An Efficient Unification Algorithm"

### Resolution
- Kowalski, R. A. (1974). "Predicate Logic as Programming Language"
- Lloyd, J. W. (1987). "Foundations of Logic Programming"

### Parsing
- Aho, A. V., et al. (2006). "Compilers: Principles, Techniques, and Tools"
- Appel, A. W. (1998). "Modern Compiler Implementation in ML"

### Lambda Calculus
- Krivine, J. L. (2007). "A call-by-name lambda-calculus machine"
- Barendregt, H. P. (1984). "The Lambda Calculus: Its Syntax and Semantics"

---

## Acknowledgments

This project implements:
- **Unification**: Based on Robinson's algorithm with occur check
- **Resolution**: SLD-resolution for Horn clauses
- **Parsing**: Using Ocamllex and Ocamlyacc tools
- **Lambda Calculus**: Krivine machine for lazy evaluation

---

## License and Usage

This code is provided for educational purposes as part of coursework assignments.

---

**End of Documentation**
