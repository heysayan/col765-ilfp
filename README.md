# col765-ilfp
Course COL765: Introduction to Logic and Functional Programming coursework, under Prof. Sanjiva Prasad at IIT-Delhi

## Assignment 1: Programming with Relations
The purpose of this assignment is to get you to start programming with Prolog, and get familiar with its syntax and execution model.   Since this assignment is given early in the course, it is meant to be a little light and provide you with some recreation.   Please try to pick very different data sets from your friends.  My goal is to have a large collection of culturally interesting genealogy databases, so novelty will be rewarded.

Take a reasonably large family from a book or soap opera or movie franchise, with many characters (at least 40) and at least 3-4 generations.

The more complicated the family structure, and the more the marriages and children, the better.

Some examples are:

The Mahabharata, the Bible (OT), The Ramayana, Star Wars, The Lord of the Rings, Greek mythology, Roman mythology, Egyptian mythology, Viking Sagas, Nibelungenlied, One Hundred Years of Solitude, Midnight's Children, Dynasty, Kyunki Saas Bhi Kabhi Bahu Thi, ...

Or  genealogies of various historical dynasties (though these usually only record the male descendants).

Or movie stars (in Bollywood, or Hollywood,...)

Create a database using the basic predicates: male(Y), female(X), child(X,Y), married(A,B).

Define as many relationships as you can think of:  son, daughter, uncle, aunt, grandfather, nephew, cousin, sister-in-law, mother-in-law, daughter-in-law, etc.  In fact, I encourage you to use the word in your mother tongue for that relationship.

Formulate and run queries (and support your answers by referencing pages from texts, or hyperlinks to wikipedia).  

Try different input/output modes.

## Assignment 2: Vectors represented using lists 
**Vectors**

In this assignment, you will have to model n-dimensional vectors using OCaml lists over the OCaml type float.  (Please read the online documentation on OCaml about the float type, used for representing Real numbers.  Integers are not automatically coerced into floats.  And the operations of addition etc. are written slightly differently -- gor example,  addition is written +. and *. is used for multiplication of floats.)

You should use as many functions provided by the OCaml  List module, particularly map and fold_left etc.



1. Define a type called vector  -- an n-dimensional vector will be represented as a float list of length n).
2. Also define an exception DimError which is raised when arguments to an operation do not have the corrected dimension(s).

Now you will have to implement the following operations:

1. zero:  int ->  vector,  given a positive integer n, return the representation of the zero vector of fixed dimension n.
2. How will you represent the unit vectors of an n-dimensional vector space on a standard basis (where the axes are assumed orthogonal to one another)?
3. dim:  vector -> int, which given a vector, v   returns its dimension.
4. opp: vector -> vector, which given a vector v, returns a vector of the same dimension and  magnitude, but in the opposite direction.
5. addv: vector -> vector -> vector,  which given vectors v_1 and v_2 of the same dimension, returns their sum vector v_1+v_2.     Check that your implementation satisfies  commutativity, associativity,  identity and inverse laws of vector addition.
6. subv: vector -> vector -> vector,  which given vectors v_1 and v_2 of the same dimension, returns the vector v_1 - v_2.
7. scalarmult: float -> vector -> vector, which given a float c and  vector v, returns the vector cv, that is v with each component scaled by c.  Verify that your implementation ensures that scalar multiplication distributes over vector addition
8. dotprod:  vector -> vector -> float, which given two  vectors v_1 and v_2 of the same dimension, returns their scalar product or dot product v_1 • v_2.  Is dotprod commutative?
9. norm:  vector -> float, which given a vector v, returns its magnitude || v ||. 
10. normalise: vector -> vector, that given a vector v, returns a vector of unit magnitude but in the same direction. 
11. parallel: vector -> vector -> bool, which given two  vectors v_1 and v_2 of the same dimension, checks whether  v_1 and v_2 are parallel (i.e., in the same direction, but possibly not of the same magnitude) or not.
12. \[For 3-dimensional vectors only.\]  crossprod: vector -> vector -> vector, where given 3-dimensional vectors v_1 and v_2 (in the standard basis), returns  the 3-dimensional vector that is their cross product v_1 x v_2.  Verify that your implementation satisfies the laws of cross product -- that it is, e.g., v_2 x v_1  = opp (v_1 x v_2) 
13. [BONUS} rotate: vector -> (vector list) -> vector, which given an n-dimensional vector v (on a standard basis), and n linearly independent basis vectors [v_1, …, v_n], returns the representation of vector v but in the new basis. How can you check if v_1, …, v_n are linearly independent of one another?
14. {BONUS]  Define the box product of three vectors v_1, v_2, v_3, defined as the scalar v_1 • (v_2 x v_3)


## Assignment 3: A simple boolean evaluator and compiler
In this assignment, you will be expected to model a simple language of boolean expressions, and write an evaluator as well as a compiler to opcodes evaluated using a stack machine. 

1. Define a boolean expression language given by the data type expb, to include operations such as constants T and F,  (unary) negation Not, and binary operations of conjunction And, disjunction Or (and implication Imply and  Bi-implication Iff, and any other expressions which you can think of.  

2.  Next extend the definitional interpreter evalb to provide a standard semantics to all these boolean expressions.  

3.  Then define a data type opcodeb to encode operations on the booleans, 

4. Define the function compileb to generate code for boolean expressions in terms of the opcodes.

5. Adapt the definition of the stack machine execution to define a function stkmcb by providing execution rules for the operations you have defined above.

6. State the theorems that your compile-and-execute (on the stack machine) implementation of these operations is correct  (sound and complete) with respect to the standard semantics given by evalb.

7. Bonus:  Prove these theorems.


## Assignment 5: Terms, Substitutions and Unifiers
Consider the representation of trees ("pre-terms") using the following data type definition

type tree = V of string | C of { node: symbol ; children: tree list };;

with suitable type representations for types  symbol and signature.

1. Given a signature consisting of symbols and their arities (>= 0) in any suitable form -- either as a list of (symbol, arity) pairs, or as a function from symbols to arities, write a function check_sig that checks whether the signature is a valid signature (no repeated symbols, arities are non-negative etc.)  Use map, fold_left etc as appropriate.
2. Given a valid signature (checked using check_sig), define a function wftree that checks that a given tree (preterm is well-formed according to the signature.
3. Define functions ht, size and vars that given a well-formed tree, return its height, its size and the set of variables appearing in it respectively.  Use map, fold_left and other such functions as far as possible wherever you use lists.  (these have all been defined in class; but come up with representative tests, which can work for the subsequent programs).
4. Write a function mirror, which given a tree t returns a tree that is the mirror image of t.  That is, at each level for each node, its children are reversed.
5. Define a suitable representation for substitutions as a table defined as a list of pairs (check that the table is a valid representation of a function).  Come up with an efficient representation of composition of substitutions. 
6. Define the function subst that given a tree t and a (table-form) substitution s, applies the (Unique Homomorphic Extension of) s to t.  Ensure that subst is efficiently implemented.  (One version of subst is already been defined in class.)
7. Define the function mgu that given two well formed trees (terms) t1 and t2, returns their most general unifier, if it exists and otherwise raises an exception NOT_UNIFIABLE.
8. Provide at least 8 test cases to show that mgu works correctly, clearly showing that your examples cover all cases in the analysis.
 Check if  mgu (t, u) = mgu (mirror u, mirror t).

 
## Assignment 6: Propositional Resolution Engine
In this assignment, you will write in OCaml a simplified version of a SLD Propositional Resolution algorithm for Horn Clauses.

You will first define an OCaml data type to represent the structure of a legitimate Horn Clause propositional program.

A program is a sequence of clauses. 
 A clause can either be a fact or a rule. A fact has a head but no body.  A rule has a head and a body.  
The head is a single proposition.  A body is a sequence of propositions.
A goal is a  sequence of propositions (referred to as subgoals).


Program execution proceeds as follows:

Replace the first subgoal (proposition) from a list of goals by the list of propositions obtained by resolving it (the first subgoal) with the head of a program clause.

You also need to develop a back-tracking strategy to explore the resolution search space, when a sub-goal cannot be satisfied.

The only control data structures you need are lists: whether as stacks or queues (and perhaps indexes into them).



## Assignment 7: A Toy Prolog Interpreter
You have previously implemented terms, substitutions and unification. 

You have also implemented a Propositional Resolution Engine with back-tracking.

Now, you should be able to combine these in a modular fashion (replacing parts of your code with other earlier written code).

In this assignment, you will write a simplified version of a Prolog interpreter in OCaml.

You will first define an OCamL data type to represent the abstract structure of a legitimate Prolog program (you may first assume that the ASTs will be well-formed to get your interpreter working, then later ensure that they indeed are)

- A program is a set (list) of clauses. 
- A clause can either be a fact or a rule. A fact has a head but no body.  A rule has a head and a body.  
- The head is a single atomic formula.  A body is a sequence of atomic formulas.
- An atomic formula is a k-ary predicate symbol followed by k terms.
- A term is either a variable, a constant, or a k-ary function symbol with k subterms.
- A goal is a sequence of atomic formulas.

You need to take your implementation of unification and use it as the parameter-passing mechanism. (Note: by pretending the predicate symbol is a function symbol, you can perform resolution of goals and program clauses).

You also need to modify your back-tracking strategy to explore the resolution search space (reverting to an older substitution).   You need to be able to replace a goal by subgoals, as found by applying a unifier to the body of a program clause whose head unified with the chosen subgoal.

Test your toy interpreter out on your Prolog program.   For that you may quickly write a translator from Prolog syntax to the data type for ASTs defined above. 


## Assignment 8: CBN Closure Machine and Krivine Machine
In this assignment, you will have to implement an abstract machine for a toy functional language based on the Call-by-Name  lambda calculus.

The abstract syntax for lambda expressions  e ::= x |  (e1 e2) | \x.e  (where \x.e1 depicts  "\lambda x . e1") can be coded in OCaml as:

type lamexp = V of string |  App of lamexp * lamexp  | Lam of string * lamexp ;;

First, write an OCaml program to implement substitution in the lambda calculus correctly. You may assume an ordering of variables, and generate a unique fresh variable each time to be different from any variable used.

Second, implement the Krivine machine  for call-by-name  for  the pure lambda calculus (First, assume closed terms; then see if your program works for open terms).

Let closures be depicted as <<e, gamma>> where 

e is a lambda expression

gamma is a table of type  string -> closure



Let f[ x |-> v ] denote extending/modifying function f at the entry for x.



(Op) << (e1 e2), gamma>>, S   =>  <<e1, gamma>> ,  <<e1, gamma>>::S

(Var)  << x, gamma>>, S   =>  gamma(x),  S

(App)  << \x.e1, gamma>>, cl::S => <<e1, \gamma[x |-> cl] >>, S



You need to:

Define closures appropriately
Implement the execution of the Krivine machine until it cannot make any further steps.
Implement the "unload" function, that takes a Krivine machine configuration and unravels the resulting closure (and any stacked arguments) into a lambda term. 
Again, you need to provide enough input cases to show your program runs correctly.  Suggestion: You may use some of the examples that encode pairing and conditionals. 

