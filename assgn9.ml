(* Main interpreter file for the Prolog-like resolution engine *)
(* This file integrates the lexer, parser, and resolution engine *)

open List
open Printf

(* Re-export types from parser for compatibility with assgn7 *)
type symbol = Parser.symbol
type tree = Parser.tree
type formula = Parser.formula
type hornClause = Parser.hornClause
type program = Parser.program

(* Type definitions from assgn7 for substitutions *)
type substitution = (string * tree) list

(* ============= Resolution Engine Functions ============= *)
(* These are imported from assgn7.ml logic *)

(* Utility: Check if tree is well-formed *)
let rec wftree (t:tree) : bool = match t with
    Parser.V x -> if (x="") then false else true
  | Parser.C r -> match r.node with (sym,arity) ->
      if (length r.children = arity)
      then (fold_left (&&) true (map wftree r.children))
      else false
;;

(* Get variables from a tree *)
let vars (t:tree) : string list =
    let rec vars_tail (v:string list) (t_:tree) =
    (
    match t_ with
        Parser.V x -> if (mem x v) then v else (x::v)
      | Parser.C r ->(
              let temp_fun = (fold_left (fun (x:(tree->string list)) y ->
                vars_tail (x y) ) (vars_tail v) r.children)
              in (match (temp_fun (Parser.V "temp")) with head::tail -> tail | _ -> [] )
              )
    ) in vars_tail [] t
;;

(* Apply substitution to a tree *)
let rec subst (s:substitution) (t:tree) : tree =
  let rec lookup (name:string) (s:substitution) : tree = ( match s with
        (var_name,value)::tail -> if (name=var_name) then value else (lookup name tail)
      | _ -> Parser.V name )
      in
  match t with
      Parser.V x -> (lookup x s)
    | Parser.C r -> Parser.C {node=r.node;children=(map (subst s) r.children)}
;;

(* Compose two substitutions *)
let compose_subst (s1:substitution) (s2:substitution) : substitution =
  let rec compose_subst_in acc sub1 sub2 =
    (
    match sub1 with
        (name,value)::tail -> let v2 = (subst s2 value) in
                                  compose_subst_in ((name,v2)::acc) tail s2
      | [] -> acc@(
                  filter
                      (
                        fun (n,v) -> (find_opt (fun (n_,v_)->n_=n) acc)=None
                      )
                  s2
                  )
    )
    in filter (fun (n,v) -> not (v = Parser.V n)) (compose_subst_in [] s1 s2)
;;

(* Most General Unifier *)
exception NOT_UNIFIABLE
let rec mgu (t1:tree) (t2:tree) : substitution = match t1 with
    Parser.V x -> (
            match t2 with
              Parser.V y -> [(x,Parser.V y)]
             | Parser.C r -> if (mem x (vars t2)) then (raise NOT_UNIFIABLE) else [(x,t2)]
           )
  | Parser.C r -> (
            match t2 with
              Parser.V y -> if (mem y (vars t1)) then (raise NOT_UNIFIABLE) else [(y,t1)]
             | Parser.C r_ -> (
                      if not(r.node=r_.node) then (raise NOT_UNIFIABLE) else
                        fold_left (
                                  fun mgu_ (ct1,ct2) -> compose_subst mgu_ (mgu (subst mgu_ ct1) (subst mgu_ ct2))
                                  ) [] (combine r.children r_.children)
                     )
           )
;;

(* Formula utilities *)
let rec wfformula (f:formula) : bool =
  match f with
        Parser.T -> true
    |   Parser.F -> true
    | Parser.Pred r -> (match r.predicate with (sym,arity) ->
                  (if (length r.members = arity) then (fold_left (&&) true (map wftree r.members)) else false))
    | Parser.And (f1,f2) -> (wfformula f1)&&(wfformula f2)
    | Parser.Or (f1,f2) -> (wfformula f1)&&(wfformula f2)
    | Parser.Implies (f1,f2) -> (wfformula f1)&&(wfformula f2)
    | Parser.Iff (f1,f2) -> (wfformula f1)&&(wfformula f2)
    | Parser.Not f1 -> (wfformula f1)
;;

let rec formula_vars (f:formula) : string list =
  match f with
        Parser.T -> []
    |   Parser.F -> []
    | Parser.Pred r -> ( fold_left (fun x y-> ( fold_left (fun a b -> b::a) x (filter (fun p -> not (mem p x)) y)) ) [] (map (vars) r.members) )
    | Parser.And (f1,f2) -> (fold_left (fun x y-> if (mem y x) then x else y::x) (formula_vars f1) (formula_vars f2))
    | Parser.Or (f1,f2) -> (fold_left (fun x y-> if (mem y x) then x else y::x) (formula_vars f1) (formula_vars f2))
    | Parser.Implies (f1,f2) -> (fold_left (fun x y-> if (mem y x) then x else y::x) (formula_vars f1) (formula_vars f2))
    | Parser.Iff (f1,f2) -> (fold_left (fun x y-> if (mem y x) then x else y::x) (formula_vars f1) (formula_vars f2))
    | Parser.Not f1 -> (formula_vars f1)
;;

let rec formula_subst (s:substitution) (f:formula) : formula =
  match f with
        Parser.T -> Parser.T
    |   Parser.F -> Parser.F
    | Parser.Pred r -> Parser.Pred {predicate=r.predicate;members=(map (subst s) r.members)}
    | Parser.And (f1,f2) -> Parser.And (formula_subst s f1,formula_subst s f2)
    | Parser.Not f1 -> Parser.Not (formula_subst s f1)
    | Parser.Or (f1,f2) -> Parser.Or (formula_subst s f1,formula_subst s f2)
    | Parser.Implies (f1,f2) -> Parser.Implies (formula_subst s f1,formula_subst s f2)
    | Parser.Iff (f1,f2) -> Parser.Iff (formula_subst s f1,formula_subst s f2)
;;

let rec formula_mgu (f1:formula) (f2:formula) : substitution = match f1 with
    Parser.T -> (match f2 with
            Parser.T -> []
          | _ -> raise NOT_UNIFIABLE)
  | Parser.F -> (match f2 with
            Parser.F -> []
          | _ -> raise NOT_UNIFIABLE)
  | Parser.Pred r -> (
            match f2 with
             Parser.Pred r_ -> (
                      if not(r.predicate=r_.predicate) then (raise NOT_UNIFIABLE) else
                        fold_left (
                                  fun mgu_ (ct1,ct2) -> compose_subst mgu_ (mgu (subst mgu_ ct1) (subst mgu_ ct2))
                                  ) [] (combine r.members r_.members)
                     )
            | _ -> raise NOT_UNIFIABLE
           )
  | Parser.Not f11 -> (match f2 with
                Parser.Not f22 -> formula_mgu f11 f22
              | _ -> raise NOT_UNIFIABLE)
  | Parser.And (f11,f12) ->
        (
        match f2 with
                Parser.And (f21,f22) ->
                    (
                        let s1 = (formula_mgu f11 f21) in
                        (compose_subst s1 (formula_mgu (formula_subst s1 f12) (formula_subst s1 f22)) )
                    )
              | _ -> raise NOT_UNIFIABLE
        )
  | Parser.Or (f11,f12) ->
        (
        match f2 with
                Parser.Or (f21,f22) ->
                    (
                        let s1 = (formula_mgu f11 f21) in
                        (compose_subst s1 (formula_mgu (formula_subst s1 f12) (formula_subst s1 f22)) )
                    )
              | _ -> raise NOT_UNIFIABLE
        )
  | Parser.Implies (f11,f12) ->
        (
        match f2 with
                Parser.Implies (f21,f22) ->
                    (
                        let s1 = (formula_mgu f11 f21) in
                        (compose_subst s1 (formula_mgu (formula_subst s1 f12) (formula_subst s1 f22)) )
                    )
              | _ -> raise NOT_UNIFIABLE
        )
  | Parser.Iff (f11,f12) ->
        (
        match f2 with
                Parser.Iff (f21,f22) ->
                    (
                        let s1 = (formula_mgu f11 f21) in
                        (compose_subst s1 (formula_mgu (formula_subst s1 f12) (formula_subst s1 f22)) )
                    )
              | _ -> raise NOT_UNIFIABLE
        )
;;

let rec formula_equality (f1:formula) (f2:formula) : bool =
  let rec tree_equality (t1:tree) (t2:tree) : bool = match t1 with
     Parser.V x -> if (t2=Parser.V x) then true else false
   | Parser.C r1 -> match t2 with
                (Parser.C r2) -> if (r1.node=r2.node) then (fold_left (fun b (t_1,t_2)-> b&&(tree_equality t_1 t_2)) true (combine r1.children r2.children)) else false
              | _ -> false
  in
  match f1 with
     Parser.T -> ( if (f2=Parser.T) then true else false )
   | Parser.F -> (if (f2=Parser.F) then true else false )
   | Parser.Pred p1 ->
      (match f2 with
                  (Parser.Pred p2) ->
                    (
                    if (p1.predicate=p2.predicate) then (fold_left (fun b (t_1,t_2)-> b&&(tree_equality t_1 t_2)) true (combine p1.members p2.members)) else false
                    )
                | _ -> false
      )
   | Parser.And (f11,f12) ->
      (match f2 with
                        Parser.And (f21,f22) -> (formula_equality f11 f21) && (formula_equality f12 f22)
                      | _ -> false
      )
   | Parser.Or (f11,f12) ->
      (match f2 with
                        Parser.Or (f21,f22) -> (formula_equality f11 f21) && (formula_equality f12 f22)
                      | _ -> false
      )
   | Parser.Implies (f11,f12) ->
      (match f2 with
                        Parser.Implies (f21,f22) -> (formula_equality f11 f21) && (formula_equality f12 f22)
                      | _ -> false
      )
   | Parser.Iff (f11,f12) ->
      (match f2 with
                        Parser.Iff (f21,f22) -> (formula_equality f11 f21) && (formula_equality f12 f22)
                      | _ -> false
      )
   | Parser.Not f11 ->
      (match f2 with
                        Parser.Not f22 -> (formula_equality f11 f22)
                      | _ -> false
      )
;;

(* Check program well-formedness *)
type predicateSignature = symbol list
type signature = symbol list

exception ILLEGAL_GOAL
exception ILLEGAL_PROGRAM

let rec check_program (prog:program) (sig1:predicateSignature) (sig2:signature) : bool*predicateSignature*signature =
  let rec tree_check (s:signature) (t:tree) : bool*signature =
    (match t with
       Parser.V x -> if ((find_opt (fun (name,arity)-> name=x) s)=None) then (true,s) else (false,s)
     | Parser.C r -> (
        match r.node with (name,arity) ->
          if (arity=length r.children) then ( let search = (find_opt (fun (n,a)-> n=name) s) in
                                              match search with
                                                 None ->( let s = (r.node)::s in
                                                        (fold_left (fun (b,l) t1 -> match (tree_check l t1) with (b1,s1) -> (b&&b1,s1) ) (true,s) r.children)
                                                       )
                                               | Some (n1,ar1) -> (if (ar1=arity) then
                                                  (fold_left  (fun (b,l) t1 -> match (tree_check l t1) with (b1,s1) -> (b&&b1,s1) ) (true,s) r.children)
                                                  else (false,s))
                                            ) else ((false,s))
      )
    ) in
  let rec formula_check (s1:predicateSignature) (s2:signature) (f:formula) : bool*predicateSignature*signature =
    (match f with
        Parser.T -> (true,s1,s2)
      | Parser.F -> (true,s1,s2)
      | Parser.Pred p -> ( match p.predicate with (pred_,arity) ->
                      if (arity = length p.members) then ( let search = (find_opt (fun (n,a)-> n=pred_) s1) in
                                                            match search with
                                                               None -> (let s1 = (p.predicate::s1) in
                                                                (fold_left (fun (b,s1_,s2_) t1-> match (tree_check s2_ t1) with (b1,s2__) -> (b&&b1,s1_,s2__)) (true,s1,s2) p.members)
                                                              )
                                                             | Some (n,a) -> (if (a=arity) then
                                                                  (fold_left (fun (b,s1_,s2_) t1-> match (tree_check s2_ t1) with (b1,s2__) -> (b&&b1,s1_,s2__))
                                                                  (true,s1,s2) p.members)
                                                                  else (false,s1,s2))
                      ) else (false,s1,s2)
      )
      | Parser.Not f1 -> (formula_check s1 s2 f1)
      | Parser.And (f1,f2) -> (match (formula_check s1 s2 f1) with
                          (b,s1_,s2_) -> match (formula_check s1_ s2_ f2) with (b_,s1__,s2__) -> (b&&b_,s1__,s2__)
                          )
      | Parser.Or (f1,f2) -> (match (formula_check s1 s2 f1) with
                          (b,s1_,s2_) -> match (formula_check s1_ s2_ f2) with (b_,s1__,s2__) -> (b&&b_,s1__,s2__)
                          )
      | Parser.Implies (f1,f2) -> (match (formula_check s1 s2 f1) with
                          (b,s1_,s2_) -> match (formula_check s1_ s2_ f2) with (b_,s1__,s2__) -> (b&&b_,s1__,s2__)
                          )
      | Parser.Iff (f1,f2) -> (match (formula_check s1 s2 f1) with
                          (b,s1_,s2_) -> match (formula_check s1_ s2_ f2) with (b_,s1__,s2__) -> (b&&b_,s1__,s2__)
                          )
    ) in
  let check_hornclause (s1:predicateSignature) (s2:signature) (h1:hornClause) : bool*predicateSignature*signature =
    (match h1 with
       Parser.Fact f1 -> (formula_check s1 s2 f1)
     | Parser.Rule (head,body) -> ( fold_left (fun (b,s3,s4) f1 -> match (formula_check s3 s4 f1) with (b1,s5,s6)->(b&&b1,s5,s6)) (true,s1,s2) (head::body)
      )
     | Parser.Goal goals -> ( fold_left (fun (b,s3,s4) f1 -> match (formula_check s3 s4 f1) with (b1,s5,s6)->(b&&b1,s5,s6)) (true,s1,s2) goals
      )
    ) in
  match prog with
     h1::rest -> (match (check_hornclause sig1 sig2 h1) with
                  (b,s3,s4) -> match (check_program rest s3 s4) with (b1,s5,s6) -> (b&&b1,s5,s6)
                  )
   | [] -> (true,sig1,sig2)
;;

(* List implementation for resolution *)
type 'a list_ = Empty | Append of ('a list_*'a)
let rec list_to_list (l_:'a list_) (l:'a list) : 'a list =
  match l_ with
     Append (rest,last) -> list_to_list rest (last::l)
   | Empty -> l
;;

(* The main resolution algorithm *)
let rec resolver (p:program) (g:hornClause) : bool*(substitution list) =
  let rec traverser (prog:program) (goal:formula) (final:((formula list)*(substitution)) list_) : ((formula list)*(substitution)) list_ =
    ( match prog with
        (Parser.Fact fact)::rest_p ->
          ( try (
                  let s1 = (formula_mgu goal fact) in ( traverser rest_p goal (Append (final,([],s1))) )
                ) with NOT_UNIFIABLE -> (traverser rest_p goal final)
          )
      | (Parser.Rule (head,body))::rest_p ->
          ( try (
                  let s1 = (formula_mgu goal head) in
                  let new_goals = (map (formula_subst s1) body) in
                  ( traverser rest_p goal (Append (final,(new_goals,s1))) )
                ) with NOT_UNIFIABLE -> (traverser rest_p goal final)
          )
      | [] -> final
      | _ -> raise ILLEGAL_PROGRAM
    )
  in
  let rec resolver_in (prog:program) (goals:formula list) (sub:substitution) (final:substitution list_): bool*(substitution list_) =
    (match goals with
       g1::rest_goals ->
        ( let rec caller (res:((formula list)*(substitution)) list) (final:substitution list_) (resultb:bool): bool*(substitution list_) =
            ( match res with
                (new_goals,s1)::rest_res -> ( let goals = (
                                                              fold_right (fun a b-> a::b)
                                                              (filter (fun x->(find_opt (fun y->formula_equality x y) rest_goals)=None
                                                                      ) new_goals
                                                              ) (map (formula_subst s1) rest_goals)
                                                          ) in
                                              let out = (resolver_in prog goals (compose_subst sub s1) final) in
                                              match out with
                                                 (true,subs) -> (caller rest_res subs true)
                                               | _ -> (caller rest_res final (resultb||false) )
                                            )
             | [] -> (resultb,final)
            ) in let resolutions = (traverser prog g1 Empty) in let out = (caller (list_to_list resolutions []) final false) in
              out
        )
     | [] -> ( true,(Append (final,sub)) )
    ) in
    let (b,s1,s2) = (check_program p [] []) in
    let (b2,_,_) = (check_program [g] s1 s2) in
  if (not b) then (raise ILLEGAL_PROGRAM) else
  if (not b2) then (raise ILLEGAL_GOAL) else
  match g with
     Parser.Goal goals ->
        ( let relevant_vars =( fold_left (fun x y-> (fold_left (fun a b->if (mem b a) then a else b::a) x y) ) [] (map (formula_vars) goals) ) in
          let purge vars sub = (filter (fun (x,t)-> if (mem x vars) then true else false) sub) in
          (match (resolver_in p goals [] Empty) with (resultb,subs) -> (resultb,map (purge relevant_vars) (list_to_list subs [])))
        )
   | _ -> raise ILLEGAL_GOAL
;;

(* ============= Pretty Printing ============= *)

let rec tree_to_string (t:tree) : string =
  match t with
    Parser.V x -> x
  | Parser.C r ->
      let (name, arity) = r.node in
      if arity = 0 then name
      else name ^ "(" ^ (String.concat ", " (List.map tree_to_string r.children)) ^ ")"
;;

let rec formula_to_string (f:formula) : string =
  match f with
    Parser.T -> "T"
  | Parser.F -> "F"
  | Parser.Pred p ->
      let (name, arity) = p.predicate in
      if arity = 0 then name
      else name ^ "(" ^ (String.concat ", " (List.map tree_to_string p.members)) ^ ")"
  | Parser.Not f1 -> "Not " ^ (formula_to_string f1)
  | Parser.And (f1, f2) -> "(" ^ (formula_to_string f1) ^ " And " ^ (formula_to_string f2) ^ ")"
  | Parser.Or (f1, f2) -> "(" ^ (formula_to_string f1) ^ " Or " ^ (formula_to_string f2) ^ ")"
  | Parser.Implies (f1, f2) -> "(" ^ (formula_to_string f1) ^ " Implies " ^ (formula_to_string f2) ^ ")"
  | Parser.Iff (f1, f2) -> "(" ^ (formula_to_string f1) ^ " Iff " ^ (formula_to_string f2) ^ ")"
;;

let hornclause_to_string (h:hornClause) : string =
  match h with
    Parser.Fact f -> (formula_to_string f) ^ "."
  | Parser.Rule (head, body) ->
      (formula_to_string head) ^ " :- " ^
      (String.concat ", " (List.map formula_to_string body)) ^ "."
  | Parser.Goal goals ->
      "Q= " ^ (String.concat ", " (List.map formula_to_string goals)) ^ "."
;;

let substitution_to_string (s:substitution) : string =
  if s = [] then "{}"
  else
    "{" ^ (String.concat ", " (List.map (fun (v, t) -> v ^ " = " ^ (tree_to_string t)) s)) ^ "}"
;;

let print_substitutions (subs:substitution list) : unit =
  if subs = [] then
    printf "No solutions found.\n"
  else begin
    printf "Solutions:\n";
    List.iteri (fun i sub ->
      printf "  %d. %s\n" (i+1) (substitution_to_string sub)
    ) subs
  end
;;

(* ============= Main Functions ============= *)

(* Parse a string into a program *)
let parse_string (s:string) : program =
  let lexbuf = Lexing.from_string s in
  try
    Parser.program Lexer.token lexbuf
  with
  | Parsing.Parse_error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      failwith (sprintf "Parse error at line %d, column %d"
        pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol))
  | Failure msg -> failwith msg
;;

(* Parse a file into a program *)
let parse_file (filename:string) : program =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  try
    let prog = Parser.program Lexer.token lexbuf in
    close_in ic;
    prog
  with
  | Parsing.Parse_error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      close_in ic;
      failwith (sprintf "Parse error in file %s at line %d, column %d"
        filename
        pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol))
  | e -> close_in ic; raise e
;;

(* Run a query on a program *)
let run_query (prog:program) (query:hornClause) : unit =
  printf "Query: %s\n" (hornclause_to_string query);
  try
    let (success, subs) = resolver prog query in
    if success then begin
      printf "Success!\n";
      print_substitutions subs
    end else begin
      printf "Failed: No solutions found.\n"
    end
  with
  | ILLEGAL_PROGRAM -> printf "Error: Illegal program\n"
  | ILLEGAL_GOAL -> printf "Error: Illegal goal\n"
  | e -> printf "Error: %s\n" (Printexc.to_string e)
;;

(* Extract program and goals from a parsed program *)
let split_program (prog:program) : program * (hornClause list) =
  let rec aux facts_rules goals = function
    | [] -> (List.rev facts_rules, List.rev goals)
    | (Parser.Goal _ as g) :: rest -> aux facts_rules (g :: goals) rest
    | h :: rest -> aux (h :: facts_rules) goals rest
  in
  aux [] [] prog
;;

(* Run all queries in a program file *)
let run_program_file (filename:string) : unit =
  printf "Loading program from: %s\n\n" filename;
  let full_prog = parse_file filename in
  let (prog, goals) = split_program full_prog in

  printf "Program loaded successfully.\n";
  printf "Facts and Rules:\n";
  List.iter (fun h -> printf "  %s\n" (hornclause_to_string h)) prog;
  printf "\n";

  if goals = [] then
    printf "No queries found in the program.\n"
  else begin
    printf "Running %d queries...\n\n" (List.length goals);
    List.iteri (fun i goal ->
      printf "========== Query %d ==========\n" (i+1);
      run_query prog goal;
      printf "\n"
    ) goals
  end
;;

(* Interactive REPL *)
let repl () =
  printf "Prolog-like Resolution Engine REPL\n";
  printf "Enter facts, rules, and queries. End each with a dot (.)\n";
  printf "Type 'quit.' to exit.\n\n";

  let rec loop prog =
    printf "?- ";
    flush stdout;
    try
      let line = read_line () in
      if String.trim line = "quit." then
        printf "Goodbye!\n"
      else begin
        let parsed = parse_string line in
        match parsed with
        | [Parser.Goal _ as g] ->
            run_query prog g;
            loop prog
        | clauses ->
            let new_prog = prog @ clauses in
            printf "Added %d clause(s) to the program.\n" (List.length clauses);
            loop new_prog
      end
    with
    | End_of_file -> printf "\nGoodbye!\n"
    | Failure msg -> printf "Error: %s\n\n" msg; loop prog
    | e -> printf "Error: %s\n\n" (Printexc.to_string e); loop prog
  in
  loop []
;;

(* ============= Entry Point ============= *)

let () =
  if Array.length Sys.argv > 1 then
    (* File mode: run program from file *)
    run_program_file Sys.argv.(1)
  else
    (* Interactive mode *)
    repl ()
;;
