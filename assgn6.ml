open List;;

type prop = string;;

type hornClause = Fact of prop | Rule of prop(*head*)*(prop list)(*body*) | Goal  of (prop list(*list of subgoals*))  ;;
type program = hornClause list;;

exception ILLEGAL_GOAL of hornClause;;
exception ILLEGAL_PROGRAM of int;;
let resolver (p:program) (g:hornClause) : ( bool * ((int*hornClause) list)) =
 let rec resolver_traced (prog:program) (goals:hornClause) (traceback : (int*hornClause) list) : (bool*((int*hornClause) list))  =
 (*resolves the given goals based on given program, to check logical dependence*)
  (  let rec traverser (clause:int) (seq:program) (goal:hornClause) : (int*bool*hornClause) = (*helps to traverse program to resolve single provided goal*)
    (
      match seq with
        first_clause::rest_clauses -> (match goal with (Goal [g]) ->
                                        match first_clause with
                                            Fact head -> if (head=g) then (clause,true,Goal [])    (*given goal being resolved with a fact*)
                                                            else (traverser (clause+1) rest_clauses goal)  (*moving to next clause in program*)
                                          | Rule (head,body) -> if (head=g) then (clause,true,Goal body)   (*given goal being resolved with the head of rule*)
                                                                    else (traverser (clause+1) rest_clauses goal)  (*moving to next clause in program*)
                                          | _ -> raise (ILLEGAL_PROGRAM clause)     (*unexpected clause in program*)
                                      )
      | _ -> (clause,false,goal)
    )
    in

    match goals with
        Goal (first_goal::rest_goals) -> (
                                            match ( traverser 1 prog (Goal [first_goal]) ) with   (*traverse program to resolve first sub-goal*)
                                                (clause_count,true,Goal new_goals) -> resolver_traced prog (Goal( fold_right (fun a b->a::b)  (*current goal is resolved,new_goals being added*)
                                                                            ( filter (fun x -> (find_opt (fun y->x=y) rest_goals)=None) new_goals (*avoids redundancy*)
                                                                            ) rest_goals)) ((clause_count,goals)::traceback)
                                              | (_,false,_) -> (false,traceback)     (*current goal can't be resolved based on given program*)
                                         )

      | Goal [] -> (true,((1,Goal [])::traceback)) (*no goals remaining*)
      | _ -> raise (ILLEGAL_GOAL goals)
  ) in (resolver_traced p g [])
;;
(*NOTE: resolver resolves first sub-goal earliest, and when resolution is done using a rule, then the first sub-goal of the body of the same rule is resolved earliest.*)

(*================TEST CASES================*)

(*Program 1*)
let p1 = [];;
resolver p1 (Goal []);;        (*output = - : bool * (int * hornClause) list = (true, [(1, Goal [])])*)
resolver p1 (Goal ["A";"B"]);;     (*output = - : bool * (int * hornClause) list = (false, [])*)

(*Program 2*)
let p2 = [Fact "A";Fact "B"; Rule ("C",["A";"B"]); Rule ("D",["B";"C"]); Rule ("E",["A";"D"]); Rule ("F",["C";"E"]); Rule ("G",["C";"D"])];;
resolver p2 (Goal []) ;;        (*output = - : bool * (int * hornClause) list = (true, [(1, Goal [])])*)
resolver p2 (Goal ["F";"G"]) ;;         (*output = - : bool * (int * hornClause) list =
(true,
 [(1, Goal []); (2, Goal ["B"]); (1, Goal ["A"; "B"]); (3, Goal ["C"]);
  (2, Goal ["B"; "C"]); (4, Goal ["D"]); (2, Goal ["B"; "D"]);
  (1, Goal ["A"; "B"; "D"]); (3, Goal ["C"; "D"]); (7, Goal ["G"]);
  (2, Goal ["B"; "G"]); (1, Goal ["A"; "B"; "G"]); (3, Goal ["C"; "G"]);
  (2, Goal ["B"; "C"; "G"]); (4, Goal ["D"; "G"]); (1, Goal ["A"; "D"; "G"]);
  (5, Goal ["E"; "G"]); (2, Goal ["B"; "E"; "G"]);
  (1, Goal ["A"; "B"; "E"; "G"]); (3, Goal ["C"; "E"; "G"]);
  (6, Goal ["F"; "G"])])*)
resolver p2 (Goal ["B";"Z"]) ;;         (*output = - : bool * (int * hornClause) list = (false, [(2, Goal ["B"; "Z"])]) *)
resolver p2 (Goal ["E";"F"]) ;;     (*output = - : bool * (int * hornClause) list = (true, [(1, Goal []); (2, Goal ["B"]); (1, Goal ["A"; "B"]); (3, Goal ["C"]);
  (2, Goal ["B"; "C"]); (4, Goal ["D"]); (1, Goal ["A"; "D"]); (5, Goal ["E"]);
  (2, Goal ["B"; "E"]); (1, Goal ["A"; "B"; "E"]); (3, Goal ["C"; "E"]);
  (6, Goal ["F"]); (2, Goal ["B"; "F"]); (1, Goal ["A"; "B"; "F"]);
  (3, Goal ["C"; "F"]); (2, Goal ["B"; "C"; "F"]); (4, Goal ["D"; "F"]);
  (1, Goal ["A"; "D"; "F"]); (5, Goal ["E"; "F"])])*)
try (resolver p2 (Fact "It is an Illegal goal")) with (ILLEGAL_GOAL g) -> (false,[(1,g)]) ;; (*output = - : bool * (int * hornClause) list =
(false, (::) ((1, Fact "It is an Illegal goal"), []))*)

(*Program 3 (illegal)*)
let p3 = [Fact "A";Goal []];;
try (resolver p3 (Goal ["B"])) with (ILLEGAL_PROGRAM l) -> (false,[(l,Fact ("Illegal Clause at line "^(Int.to_string l)))]) ;;
(*output = - : bool * (int * hornClause) list =
(false, (::) ((2, Fact "Illegal Clause at line 2"), []))
*)


(*=================GIVEN TESTCASES==============*)

(* Test Case 1 *)
let prog = [Fact "a"] in
let goal = (Goal ["a"]) in
let (result,_) = resolver prog goal in
Printf.printf "Test Case 1 result: %b\n" result;;

(* Test Case 2*)
let prog = [Rule ("b", ["a"]); Fact "a"] in
let goal = (Goal ["b"]) in
let (result,_) = resolver prog goal in
Printf.printf "Test Case 2 result: %b\n" result;;

(* Test Case 3 *)
let prog = [Rule ("c", ["a"; "b"]); Fact "a"; Fact "b"] in
let goal = (Goal ["c"]) in
let (result,_) = resolver prog goal in
Printf.printf "Test Case 3 result: %b\n" result;;


(* Test Case 4*)
let prog = [Rule ("b", ["a"]); Rule ("c", ["b"]); Fact "a"] in
let goal = (Goal ["d"]) in
let (result,_) = resolver prog goal in
Printf.printf "Test Case 4 result: %b\n" result;;

(* Test Case 5*)
let prog = [Rule ("p", ["q"]); Rule ("p", ["r"]); Fact "q"; Fact "r"] in
let goal = (Goal ["p"]) in
let (result,_) = resolver prog goal in
Printf.printf "Test Case 5 result: %b\n" result;;



