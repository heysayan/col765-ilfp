open List;;
#require "str";;
type symbol = string*int;;

type signature = symbol list;;

type tree = V of string | C of {node:symbol;children:tree list};;

let vars (t:tree) : string list =
    let rec vars_tail (v:string list) (t_:tree) =
    (
    match t_ with
        V x -> if (mem x v) then v else (x::v)
      | C r ->(
              let temp_fun = (fold_left (fun (x:(tree->string list)) y -> vars_tail (x y) ) (vars_tail v) r.children)
              in (match (temp_fun (V "temp")) with head::tail -> tail | _ -> [] )
              )
    ) in vars_tail [] t
;;
let rec wftree (t:tree) : bool = match t with
    V x -> if (x="") then false else true (*unempty var_name*)
  | C r -> match r.node with (sym,arity) -> if (length r.children = arity) then (fold_left (&&) true (map wftree r.children)) else false
;;
let rec tree_equality (t1:tree) (t2:tree) : bool = match t1 with
   V x -> if (t2=V x) then true else false
 | C r1 -> match t2 with
              (C r2) -> if (r1.node=r2.node) then (fold_left (fun b (t_1,t_2)-> b&&(tree_equality t_1 t_2)) true (combine r1.children r2.children)) else false
            | _ -> false
;;
type substitution = (string*tree) list;;
let valid_substitution (sub:substitution) : bool =
    let count (name,_) s = (
                              fold_left (+) 0 (  (*Count the number of times var named 'name' appeared*)
                                              map (fun (n,v) -> if name=n then 1 else 0) s
                                              )
                             ) in
    fold_left (&&) true (  (*Checks if every variable appears once and no itself substitution*)
                          map (
                                fun (name,value) -> (not (value=V name)) && (count (name,value) sub = 1) && (wftree value)
                              ) sub
                        )
;;

let rec mirror (t:tree) : tree = match t with
    C r -> C {node=r.node;children=(map (mirror) (rev r.children))}
  | _ -> t
;;

let rec subst (s:substitution) (t:tree) : tree =
  let rec lookup (name:string) (s:substitution) : tree = ( match s with
        (var_name,value)::tail -> if (name=var_name) then value else (lookup name tail)
      | _ -> V name )
      in
  match t with
      V x -> (lookup x s)
    | C r -> C {node=r.node;children=(map (subst s) r.children)}
;;

let compose_subst (s1:substitution) (s2:substitution) : substitution =
  let rec compose_subst_in acc(*accumulator*) sub1 sub2 =
    (
    match sub1 with
        (name,value)::tail -> let v2 = (subst s2 value) in
                                  compose_subst_in ((name,v2)::acc) tail s2
      | [] -> acc@(
                  filter  (*filter substitutions common in acc and s2*)
                      (
                        fun (n,v) -> (find_opt (fun (n_,v_)->n_=n) acc)=None
                      )
                  s2
                  )
    )
    in filter (fun (n,v) -> not (v = V n)) (compose_subst_in [] s1 s2)  (*filter ensures no identity substitution*)
;;

exception NOT_UNIFIABLE;;
let rec mgu (t1:tree) (t2:tree) : substitution = match t1 with
    V x -> (
            match t2 with
              V y -> [(x,V y)]
             |C r -> if (mem x (vars t2)) then (raise NOT_UNIFIABLE) else [(x,t2)]
           )
  | C r -> (
            match t2 with
              V y -> if (mem y (vars t1)) then (raise NOT_UNIFIABLE) else [(y,t1)]
             |C r_ -> (
                      if not(r.node=r_.node) then (raise NOT_UNIFIABLE) else
                        fold_left (
                                  fun mgu_ (ct1,ct2) -> compose_subst mgu_ (mgu (subst mgu_ ct1) (subst mgu_ ct2))
                                  ) [] (combine r.children r_.children)
                     )
           )
;;



let subst_equality (s1:substitution) (s2:substitution) : bool =
  let contained (sub1:substitution) (sub2:substitution) = (fold_left (&&) true (map (fun x -> mem x sub2) sub1) )
  in (contained s1 s2)&&(contained s2 s1)
;;

let substitution_mirror (s:substitution) : substitution = (map (fun (name,value)->(name,mirror value)) s);;




(*============= FOL=============*)


type predicateSignature = symbol list;;
type formula = T | F |
                Pred of {predicate:symbol;members:tree list} |
                Not of formula | And of formula*formula | Or of formula*formula | Implies of formula*formula | Iff of formula*formula
              (* | ForAll of tree*formula | ForSome of tree*formula *)
;;
type hornClause = Fact of formula | Rule of formula(*head*)*(formula list)(*body*) | Goal  of (formula list(*list of subgoals*))  ;;
type program = hornClause list;;
exception ILLEGAL_GOAL;;
exception ILLEGAL_PROGRAM;;


let rec wfformula (f:formula) : bool =
  match f with
        T -> true
    |   F -> true
    | Pred r -> (match r.predicate with (sym,arity) ->
                  (if (length r.members = arity) then (fold_left (&&) true (map wftree r.members)) else false))
    | And (f1,f2) -> (wfformula f1)&&(wfformula f2)
    | Or (f1,f2) -> (wfformula f1)&&(wfformula f2)
    | Implies (f1,f2) -> (wfformula f1)&&(wfformula f2)
    | Iff (f1,f2) -> (wfformula f1)&&(wfformula f2)
    | Not f1 -> (wfformula f1)
;;

let rec wfhornclause (h:hornClause) : bool =
  match h with
     Fact f1 -> (wfformula f1)
   | Rule (head,body) ->( (wfformula head)&&(fold_left (&&) true (map (wfformula) body)) )
   | Goal goals -> (fold_left (&&) true (map (wfformula) goals))
;;
let rec formula_vars (f:formula) : string list =
  match f with
        T -> []
    |   F -> []
    | Pred r -> ( fold_left (fun x y-> ( fold_left (fun a b -> b::a) x (filter (fun p -> not (mem p x)) y)) ) [] (map (vars) r.members) )
    | And (f1,f2) -> (fold_left (fun x y-> if (mem y x) then x else y::x) (formula_vars f1) (formula_vars f2))
    | Or (f1,f2) -> (fold_left (fun x y-> if (mem y x) then x else y::x) (formula_vars f1) (formula_vars f2))
    | Implies (f1,f2) -> (fold_left (fun x y-> if (mem y x) then x else y::x) (formula_vars f1) (formula_vars f2))
    | Iff (f1,f2) -> (fold_left (fun x y-> if (mem y x) then x else y::x) (formula_vars f1) (formula_vars f2))
    | Not f1 -> (formula_vars f1)
;;

let rec formula_subst (s:substitution) (f:formula) : formula =
  match f with
        T -> T
    |   F -> F
    | Pred r -> Pred {predicate=r.predicate;members=(map (subst s) r.members)}
    | And (f1,f2) -> And (formula_subst s f1,formula_subst s f2)
    | Not f1 -> Not (formula_subst s f1)
    | Or (f1,f2) -> Or (formula_subst s f1,formula_subst s f2)
    | Implies (f1,f2) -> Implies (formula_subst s f1,formula_subst s f2)
    | Iff (f1,f2) -> Iff (formula_subst s f1,formula_subst s f2)
;;



let rec formula_mgu (f1:formula) (f2:formula) : substitution = match f1 with
    T -> (match f2 with
            T -> []
          | _ -> raise NOT_UNIFIABLE)
  | F -> (match f2 with
            F -> []
          | _ -> raise NOT_UNIFIABLE)
  | Pred r -> (
            match f2 with
             Pred r_ -> (
                      if not(r.predicate=r_.predicate) then (raise NOT_UNIFIABLE) else
                        fold_left (
                                  fun mgu_ (ct1,ct2) -> compose_subst mgu_ (mgu (subst mgu_ ct1) (subst mgu_ ct2))
                                  ) [] (combine r.members r_.members)
                     )
            | _ -> raise NOT_UNIFIABLE
           )
  | Not f11 -> (match f2 with
                Not f22 -> formula_mgu f11 f22
              | _ -> raise NOT_UNIFIABLE)
  | And (f11,f12) ->
        (
        match f2 with
                And (f21,f22) ->
                    (
                        let s1 = (formula_mgu f11 f21) in
                        (compose_subst s1 (formula_mgu (formula_subst s1 f12) (formula_subst s1 f22)) )
                    )
              | _ -> raise NOT_UNIFIABLE
        )
  | Or (f11,f12) ->
        (
        match f2 with
                Or (f21,f22) ->
                    (
                        let s1 = (formula_mgu f11 f21) in
                        (compose_subst s1 (formula_mgu (formula_subst s1 f12) (formula_subst s1 f22)) )
                    )
              | _ -> raise NOT_UNIFIABLE
        )
  | Implies (f11,f12) ->
        (
        match f2 with
                Implies (f21,f22) ->
                    (
                        let s1 = (formula_mgu f11 f21) in
                        (compose_subst s1 (formula_mgu (formula_subst s1 f12) (formula_subst s1 f22)) )
                    )
              | _ -> raise NOT_UNIFIABLE
        )
  | Iff (f11,f12) ->
        (
        match f2 with
                Iff (f21,f22) ->
                    (
                        let s1 = (formula_mgu f11 f21) in
                        (compose_subst s1 (formula_mgu (formula_subst s1 f12) (formula_subst s1 f22)) )
                    )
              | _ -> raise NOT_UNIFIABLE
        )
;;



let rec formula_equality (f1:formula) (f2:formula) : bool = (*checks if ASTs of both formula are same or not*)
  match f1 with
     T -> ( if (f2=T) then true else false )
   | F -> (if (f2=F) then true else false )
   | Pred p1 ->
      (match f2 with
                  (Pred p2) ->
                    (
                    if (p1.predicate=p2.predicate) then (fold_left (fun b (t_1,t_2)-> b&&(tree_equality t_1 t_2)) true (combine p1.members p2.members)) else false
                    )
                | _ -> false
      )
   | And (f11,f12) ->
      (match f2 with
                        And (f21,f22) -> (formula_equality f11 f21) && (formula_equality f12 f22)
                      | _ -> false
      )
   | Or (f11,f12) ->
      (match f2 with
                        Or (f21,f22) -> (formula_equality f11 f21) && (formula_equality f12 f22)
                      | _ -> false
      )
   | Implies (f11,f12) ->
      (match f2 with
                        Implies (f21,f22) -> (formula_equality f11 f21) && (formula_equality f12 f22)
                      | _ -> false
      )
   | Iff (f11,f12) ->
      (match f2 with
                        Iff (f21,f22) -> (formula_equality f11 f21) && (formula_equality f12 f22)
                      | _ -> false
      )
   | Not f11 ->
      (match f2 with
                        Not f22 -> (formula_equality f11 f22)
                      | _ -> false
      )
;;



let rec check_program (prog:program) (sig1:predicateSignature) (sig2:signature) : bool*predicateSignature*signature =
  let rec tree_check (s:signature) (t:tree) : bool*signature =
    (match t with
       V x -> if ((find_opt (fun (name,arity)-> name=x) s)=None) then (true,s) else (false,s)
     | C r -> (
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
        T -> (true,s1,s2)
      | F -> (true,s1,s2)
      | Pred p -> ( match p.predicate with (pred_,arity) ->
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
      | Not f1 -> (formula_check s1 s2 f1)
      | And (f1,f2) -> (match (formula_check s1 s2 f1) with
                          (b,s1_,s2_) -> match (formula_check s1_ s2_ f2) with (b_,s1__,s2__) -> (b&&b_,s1__,s2__)
                          )
      | Or (f1,f2) -> (match (formula_check s1 s2 f1) with
                          (b,s1_,s2_) -> match (formula_check s1_ s2_ f2) with (b_,s1__,s2__) -> (b&&b_,s1__,s2__)
                          )
      | Implies (f1,f2) -> (match (formula_check s1 s2 f1) with
                          (b,s1_,s2_) -> match (formula_check s1_ s2_ f2) with (b_,s1__,s2__) -> (b&&b_,s1__,s2__)
                          )
      | Iff (f1,f2) -> (match (formula_check s1 s2 f1) with
                          (b,s1_,s2_) -> match (formula_check s1_ s2_ f2) with (b_,s1__,s2__) -> (b&&b_,s1__,s2__)
                          )
    ) in
  let check_hornclause (s1:predicateSignature) (s2:signature) (h1:hornClause) : bool*predicateSignature*signature =
    (match h1 with
       Fact f1 -> (formula_check s1 s2 f1)
     | Rule (head,body) -> ( fold_left (fun (b,s3,s4) f1 -> match (formula_check s3 s4 f1) with (b1,s5,s6)->(b&&b1,s5,s6)) (true,s1,s2) (head::body)
      )
     | Goal goals -> ( fold_left (fun (b,s3,s4) f1 -> match (formula_check s3 s4 f1) with (b1,s5,s6)->(b&&b1,s5,s6)) (true,s1,s2) goals
      )
    ) in
  match prog with
     h1::rest -> (match (check_hornclause sig1 sig2 h1) with
                  (b,s3,s4) -> match (check_program rest s3 s4) with (b1,s5,s6) -> (b&&b1,s5,s6)
                  )
   | [] -> (true,sig1,sig2)
;;



type 'a list_ = Empty | Append of ('a list_*'a) ;; (*List inductive defined on append at back*)
let rec list_to_list (l_:'a list_) (l:'a list) : 'a list =
  match l_ with
     Append (rest,last) -> list_to_list rest (last::l)
   | Empty -> l
;;

let rec resolver (p:program) (g:hornClause (*Goal*)) : bool*(substitution list) =
  let rec traverser (prog:program) (goal:formula(*single subgoal*)) (final:((formula list)*(substitution)) list_) : ((formula list)*(substitution)) list_ =
  (*helper function to search for ways to resolve a single sub-goal based on whole program*)
    ( match prog with
        (Fact fact)::rest_p ->
          ( try (
                  let s1 = (formula_mgu goal fact) in ( traverser rest_p goal (Append (final,([],s1))) )
                ) with NOT_UNIFIABLE -> (traverser rest_p goal final)
          )
      | (Rule (head,body))::rest_p ->
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
  (*main helper function that resolves all of the given subgoals*)
    (match goals with
       g1::rest_goals ->
        ( let rec caller (res:((formula list)*(substitution)) list) (final:substitution list_) (resultb:bool): bool*(substitution list_) =
            ( match res with
                (new_goals,s1)::rest_res -> ( let goals = (
                                                              fold_right (fun a b-> a::b)
                                                              (filter (fun x->(find_opt (fun y->formula_equality x y) rest_goals)=None
                                                                      ) new_goals (*To ensure the new goals are unique for old goals*)
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
  if (not b) then (raise ILLEGAL_PROGRAM) else (*checks if program is well-formed*)
  if (not b2) then (raise ILLEGAL_GOAL) else  (*checks if goal is well formed*)
  match g with
     Goal goals ->
        ( let relevant_vars =( fold_left (fun x y-> (fold_left (fun a b->if (mem b a) then a else b::a) x y) ) [] (map (formula_vars) goals) ) in
          let purge vars sub = (filter (fun (x,t)-> if (mem x vars) then true else false) sub) in
          (match (resolver_in p goals [] Empty) with (resultb,subs) -> (resultb,map (purge relevant_vars) (list_to_list subs [])))
        )
   | _ -> raise ILLEGAL_GOAL
;;
(*NOTE: resolver resolves first sub-goal earliest, and when resolution is done using a rule, then the first sub-goal of the body of the same rule is resolved earliest.*)
(*NOTE: resolver would work perfectly if there's no conflicting variable names in program as well as in goal*)




(* Function to parse arguments into constants or variables *)
let parse_argument arg =
  let is_variable arg = (* Function to check if an argument is a variable (starts with a capital letter) *)
    (String.length arg > 0 && Char.uppercase_ascii arg.[0] = arg.[0])
  in
  if (is_variable arg) then (V arg) else (C {node = (arg,0) ; children = []} )
;;



  (* Function to parse a predicate in the form of "predicate_name(arg1,arg2,...)" *)
let rec parse_predicate (pred_:string) : formula =
  let open Str in
  let pattern1 = regexp "^Not +\\(.+\\)$" in
  let pattern2 = regexp "^And *( *\\(.+ *( *.+ *)\\) *, *\\(.+ *( *.+ *)\\) *)$" in
  let pattern3 = regexp "^Or *( *\\(.+ *( *.+ *)\\) *, *\\(.+ *( *.+ *)\\) *)$" in
  let pattern4 = regexp "^Implies *( *\\(.+ *( *.+ *)\\) *, *\\(.+ *( *.+ *)\\) *)$" in
  let pattern5 = regexp "^Iff *( *\\(.+ *( *.+ *)\\) *, *\\(.+ *( *.+ *)\\) *)$" in
  let pattern6 = regexp "^\\([a-zA-Z_]+\\) *(\\(.+\\))$" in
  if (string_match pattern1 pred_ 0) then ( Not (parse_predicate (matched_group 1 pred_ |> String.trim)))
  else(
  if (string_match pattern2 pred_ 0) then ( let p1 = matched_group 1 pred_ |> String.trim in
      let p2 = (matched_group 2 pred_)|>String.trim in (And (parse_predicate p1,parse_predicate p2)) )
  else(
  if (string_match pattern3 pred_ 0 ) then ( let p1 = matched_group 1 pred_|>String.trim in
      let p2 = (matched_group 2 pred_|>String.trim) in (Or (parse_predicate p1,parse_predicate p2)) )
  else(
  if (string_match pattern4 pred_ 0 ) then ( let p1 = matched_group 1 pred_|>String.trim in
      let p2 = (matched_group 2 pred_|>String.trim) in (Implies (parse_predicate p1,parse_predicate p2)) )
  else(
  if (string_match pattern5 pred_ 0 ) then ( let p1 = matched_group 1 pred_|>String.trim in
      let p2 = (matched_group 2 pred_|>String.trim) in (Iff (parse_predicate p1,parse_predicate p2)) )
  else(
  if (string_match pattern6 pred_ 0 ) then (
    let predicate_name = (matched_group 1 pred_) |> String.trim in
    let args = matched_group 2 pred_ |> String.trim |> String.split_on_char ',' |> List.map String.trim in
    let mem = List.map parse_argument args in
    Pred { predicate = (predicate_name,(length mem)); members = mem }
  ) else(failwith ("Invalid predicate format: " ^ pred_)
  )

)
)
)
)
);;


let extract_body_predicates body =
  let rec aux acc idx depth start =
    if idx >= String.length body then
      if depth = 0 && start < idx then
        List.rev (String.sub body start (idx - start) :: acc)
      else
        List.rev acc
    else match body.[idx] with
    | '(' -> aux acc (idx + 1) (depth + 1) start
    | ')' ->
        if depth > 0 then aux acc (idx + 1) (depth - 1) start
        else if start < idx then
          let predicate = String.sub body start (idx - start + 1) in
          aux (predicate :: acc) (idx + 1) depth (idx + 1)
        else
          aux acc (idx + 1) depth start
    | ',' ->
        if depth = 0 then
          let predicate = String.sub body start (idx - start) in
          aux (predicate :: acc) (idx + 1) depth (idx + 1)
        else
          aux acc (idx + 1) depth start
    | _ -> aux acc (idx + 1) depth start
  in
  aux [] 0 0 0;;

(* Function to parse a statement (fact or rule) *)
let parse_statement (stmt:string) : hornClause =
  let stmt = (String.trim stmt) in (*let stmt = ( String.sub stmt 0 (String.length stmt - 1) )  in*)  (*removed whitespaces and ending full-stop*)
    if (stmt.[0] = 'Q' && stmt.[1]='=') then ( (*GOAL*)
        let goals = (String.sub stmt 2 (String.length stmt - 2)) |> String.trim |> extract_body_predicates |> List.map String.trim in
        let goals = List.map parse_predicate goals in
        Goal goals
    ) else (
      if String.contains stmt ':' then (  (* It's a rule*)
        let head_body = String.split_on_char ':' stmt in
        let head = List.nth head_body 0 |> String.trim in
        let body = (List.nth head_body 1) in
        let body = (String.sub body 1 (String.length body - 1)) |> String.trim |> extract_body_predicates |> List.map String.trim in
        let head_predicate = parse_predicate head in
        let body_predicates = List.map parse_predicate body in
        Rule (head_predicate, body_predicates)
      )
    else
      (* It's a fact *)
      let pred = parse_predicate stmt in
      Fact pred
    )
;;


(*translates the prolog program passed as string to datastructure of AST*)
let parse_program (prog:string) : hornClause list =
  let rec aux p = (
    match p with
       stmt1::rest -> let stmt1 = (String.trim stmt1) in
                      if (stmt1="") then (aux rest) else
                      (parse_statement stmt1)::(aux rest)
     | [] -> []
    ) in
    aux (String.split_on_char '.' prog)
;;


(*checks if given hornclauses are identical*)
let hornClause_equality h1 h2 =
  match h1 with
    Fact p1 -> (match h2 with Fact p2->(formula_equality p1 p2) | _ -> false)
  | Rule (head1,body1) ->
      (match h2 with Rule (head2,body2) -> (formula_equality head1 head2) && (fold_left (&&) true (map (fun (c1,c2) -> formula_equality c1 c2) (combine body1 body2))) | _ -> false
      )
  | Goal g1 -> (match h2 with Goal g2 -> fold_left (&&) true (map (fun (c1,c2) -> formula_equality c1 c2) (combine g1 g2)) | _ -> false )

;;


(*checks if given two program are identical*)
let rec program_equality p1 p2 =
  match p1 with
    h1::rest1 -> ( match p2 with
      h2::rest2 -> (hornClause_equality h1 h2)&&(program_equality rest1 rest2)
      | _ -> false
    )
  | [] -> (match p2 with
    [] -> true
   | _ -> false
   )
;;
(*=======================TEST CASES================*)
let p1 = parse_program ("
father(ramesh,rohan).
");;
let g1 = nth (parse_program "Q= father(ramesh,X) .") 0;;
resolver p1 g1;;
(*output = - : bool * substitution list =
(true, [[("X", C {node = ("rohan", 0); children = []})]])
*)



let p2 = parse_program ("
father (ramesh,rohan).
");;
let g2 = nth (parse_program "Q= father(ramesh,X),mother(ramesh,Z).") 0;;
resolver p2 g2;;
(*output = - : bool * substitution list = (false, [])
*)



let p3 = parse_program ("
father(ramesh,rohan).
mother(sita,rohan).
");;
let g3 = nth (parse_program "
Q= father(ramesh,X),mother(Y,rohan).
") 0;;
resolver p3 g3;;
(*output = - : bool * substitution list =
(true,
 [[("X", C {node = ("rohan", 0); children = []});
   ("Y", C {node = ("sita", 0); children = []})]])
*)



let p4  = parse_program ("
father(ramesh,rohan).
wife(sita,ramesh).
mother(X,Y) :- wife(X,Z),father(Z,Y).
");;
let g4 = nth (parse_program "
Q= father(ramesh,M),mother(N,rohan).
") 0;;
resolver p4 g4;;
(*output = - : bool * substitution list =
(true,
 [[("M", C {node = ("rohan", 0); children = []});
   ("N", C {node = ("sita", 0); children = []})]])
*)



let p5 = parse_program ("
father(ramesh,rohan).
wife(sita,ramesh).
wife(babita,ramesh).
mother(X,Y) :- wife(X,Z) , father(Z,Y).
");;
let g5 = nth (parse_program "
Q= father(ramesh,M),mother(N,rohan).
") 0;;
resolver p5 g5;;
(*output = - : bool * substitution list =
(true,
 [[("M", C {node = ("rohan", 0); children = []});
   ("N", C {node = ("sita", 0); children = []})];
  [("M", C {node = ("rohan", 0); children = []});
   ("N", C {node = ("babita", 0); children = []})]])
*)


let p6 = parse_program("
wife(sita,ram).
husband(ram,sita).
son(luv,ram).
son(luv,sita).
son(kush,ram).
son(kush,sita).
friend(hanuman,ram).
Not friend(ravan,ram).
child(A,B) :- son(A,B).
child(C,D) :- daughter(C,D).
parent(E,F) :- child(F,E).
married(G,H) :- wife(G,H).
married(I,J) :- husband(I,J).
");;


let g6 = nth (parse_program("Q= parent(M,kush).")) 0;;
resolver p6 g6;;
(*output = - : bool * substitution list =
(true,
 [[("M", C {node = ("ram", 0); children = []})];
  [("M", C {node = ("sita", 0); children = []})]])
*)


let g7 = nth (parse_program "
Q= married(W,ram) , Not friend(V,ram) , friend(K,ram) , son(O,ram).
") 0;;
resolver p6 g7;;
(*output = - : bool * substitution list =
(true,
 [[("K", C {node = ("hanuman", 0); children = []});
   ("W", C {node = ("sita", 0); children = []});
   ("V", C {node = ("ravan", 0); children = []});
   ("O", C {node = ("luv", 0); children = []})];
  [("K", C {node = ("hanuman", 0); children = []});
   ("W", C {node = ("sita", 0); children = []});
   ("V", C {node = ("ravan", 0); children = []});
   ("O", C {node = ("kush", 0); children = []})]])
*)


resolver p6 (Goal []);;
(*output = - : bool * substitution list = (true, [[]]) *)

resolver p6 (nth (parse_program "Q= child(luv,ram).") 0);;
(*output = - : bool * substitution list = (true, [[]])*)

resolver p6 (nth (parse_program "Q= child(luv,ram),child(kush,laxman).") 0);;
(*output = - : bool * substitution list = (false, [])*)

(*An ILLEGAL_PROGRAM, made from p1*)
(*let p7 = [Fact (Pred {predicate=("father",1);members=[C {node=("Ramesh",0);children=[]};C {node=("Rohan",0);children=[]}]})] ;;
resolver p7 g1;;*)
(*output = Exception: ILLEGAL_PROGRAM.*)


(*ILLEGAL GOAL being used on p6*)
(*resolver p6 (Goal [(Pred {predicate=("child",4); members=[C {node=("luv",0); children=[]}; C {node=("ram",0); children=[]}]})]);;*)
(*output = Exception: ILLEGAL_GOAL.*)




(*=============TA Testcases==============*)

let pt1 = parse_program ("
studies(charlie, csc135).
studies(olivia, csc135).
studies(jack, csc131).
studies(arthur, csc134).

teaches(kirke, csc135).
teaches(collins, csc131).
teaches(collins, csc171).
teaches(juniper, csc134).


professor(A, B) :- teaches(A, C), studies(B, C).
")
;;
(* X is a professor of Y if X teaches C and Y studies C. *)

let gt11 = nth (parse_program "Q= studies(charlie,X).") 0;;
let gt12 = nth (parse_program "Q= studies(charlie,X), studies(olivia,Y).") 0;;
let gt13 = nth (parse_program "Q= professor(kirke,olivia).") 0;;
let gt14 = nth (parse_program "Q= professor(X,arthur).") 0;;

resolver pt1 gt11;;
resolver pt1 gt12;;
resolver pt1 gt13;;
resolver pt1 gt14;;

