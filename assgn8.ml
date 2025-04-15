open List;;
#require "str";;

type lamexp = V of string |  App of lamexp * lamexp  | Lam of string * lamexp ;;


let rec free_vars (e:lamexp) : string list =
    match e with
        V x -> [x]
     | App (e1,e2) -> (let l = (free_vars e1) in
                        (
                        fold_left (fun x y-> y::x) l (filter (fun x-> not (mem x l)) (free_vars e2))
                        )
                    )
     | Lam (x,e1) -> (filter (fun y->x<>y) (free_vars e1))
;;



let fresh_var (l:lamexp list) : string =
    let rec get_max_var (m:string) (e:lamexp) : string =
        (match e with
                Lam (x,e1) -> (get_max_var m e1)
            |   V x -> (if (x>m) then x else m )
            |   App (e1,e2) -> (max (get_max_var m e1) (get_max_var m e2) )
        ) in
    let increment_var (v:string) : string = (let l = (String.length v) in
                                            let i = (Char.code v.[l-1]) in
                                            if ( (i>=97) && (i<122) ) then ( (String.sub v 0 (l-1) )^(String.make 1 (Char.chr (i+1) ) ) )
                                            else (v^"a")
                                            ) in
    let max_var = (fold_left (max) "a" (map (get_max_var "a") l)) in increment_var max_var
;;



let rec subst (v:string (*Var name*)) (e:lamexp(*substituted lamexp*)) (exp_:lamexp) : (lamexp) =
    match exp_ with
        V x -> (if (x=v) then e else  exp_)
     | App (e1,e2) -> (App (subst v e e1,subst v e e2) )
     | Lam (x,e1) -> (if (x=v) then exp_ else (
                                                let vars1 = (free_vars e) in
                                                let vars2 = (free_vars e1) in
                                                if (mem v vars2) then (
                                                                        if (mem x vars1)
                                                                        then (let z = (fresh_var [e;e1]) in (Lam (z,(subst v e (subst x (V z) e1)) )))
                                                                        else Lam (x,(subst v e e1))
                                                                      )
                                                else exp_
                                                )
                    )
;;

let rec string_to_lambda (exps:string) : lamexp =
    let open Str in
    let abst = regexp {|^ *\\\([a-zA-Z_]+\)\.\[ *\(.+\) *\] *$|} in
    let app = regexp {|^ *\([1-9]\)(\(.+\))\1 *\* *\([1-9]\)(\(.+\))\3 *$|} in
    let var = regexp {|^ *\([a-zA-Z_]+\) *$|} in

    if (string_match app exps 0) then (let e1 = (matched_group 2 exps) in let e2 = (matched_group 4 exps) in (App (string_to_lambda e1, string_to_lambda e2 )) )
    else(
    if (string_match abst exps 0) then (let v = (matched_group 1 exps) in let e = (matched_group 2 exps) in Lam (v, string_to_lambda e))
    else(
    if (string_match var exps 0) then (V (matched_group 1 exps) )
    else (failwith ("Invalid expression format: " ^ exps) )
    )
    )
;;

type closure = Closure of lamexp*((string*closure) list);;
type gamma = (string*closure) list;;



exception NOTFOUND;;
let rec lookup (v:string) (g:gamma) : closure =
    match g with
        (v1,c1)::rest -> if (v1=v) then c1 else (lookup v rest)
     | [] -> raise NOTFOUND
;;

let rec krivine_machine (stack:closure list) (focus:closure) : closure list =
    match focus with
           Closure (V x,g1) -> (try (krivine_machine stack (lookup x g1))
                                with NOTFOUND -> focus::stack
                               )
        |  Closure (Lam (x,e1) , g1) ->(match stack with
                                            top::rest -> (krivine_machine rest (Closure (e1,(x,top)::g1)) )
                                        |   [] -> (focus::stack)
                                       )
        |  Closure (App (e1,e2) , g1) ->( krivine_machine ((Closure (e2,g1))::stack) (Closure (e1,g1)) )
;;
let rec subst_closure (Closure (e,g):closure) = fold_left (fun x (v,c1)-> (subst v (subst_closure c1) x)) e g;;
exception EMPTYSTACK;;
let rec unload_stack (stack : closure list) : lamexp =
    match stack with
            (Closure (e1,g1))::((Closure (e2,g2))::rest) -> unload_stack ((Closure (App (e1,e2),g2))::rest)
        |   (Closure (e1,g1))::[] -> (subst_closure (Closure (e1,g1)) )
        |   [] -> raise EMPTYSTACK
;;

let cbn (e:lamexp) : lamexp (*returns the weak head normal form of e*) =
    krivine_machine [] (Closure (e,[])) |> try (unload_stack) with EMPTYSTACK -> failwith ("Invalid Lambda expression!")
;;


let rec lambda_to_string (e:lamexp) : string =
    match e with
            App (e1,e2) -> "( "^(lambda_to_string e1)^" ) * ( "^(lambda_to_string e2)^" )"
        |   Lam (x,e1) -> "\\"^x^".[ "^(lambda_to_string e1)^" ]"
        |   V x -> x
;;



(*==================TEST CASES====================*)

let t1 = (string_to_lambda {|1( \x.[ 2(x)2*3(y)3 ] )1 * 4( \z.[ 5(x)5*6( \y.[x] )6 ] )4|}) in
let t2 = (string_to_lambda {|1(z)1*2(y)2|}) in lambda_to_string (subst "x" t2 t1);;     (*{x:=zy}( (\x.[xy])(\z.[x(\y.z)]) ) = (\x.[xy])(\za.[zy(\za.zy)]) *)

let k = string_to_lambda {|\x.[\y.[x]]|};;      (*\x.\y.[x]*)
let s = string_to_lambda {|\x.[\y.[\z.[ 1(2(x)2*3(z)3)1 * 4(5(y)5*6(z)6)4 ]]]|};;   (*\x.\y.\z.[(xz)(yz)]*)
let omega = string_to_lambda {|\x.[1(x)1*2(x)2]|};;     (*\x.[xx]*)
let oMega = App (omega,omega);;     (*(\x.[xx])(\x.[xx])*)

cbn (App (App (k,V "z"),oMega));; (*input = (\x.\y.[x])z omega omega ; output = - : lamexp = V "z" *)


(*======test case2===*)
let t1 = string_to_lambda {|\x.[\y.[\z.[x]]]|};;    (*\x.\y.\z.[x]*)
let t2 = string_to_lambda {|\x.[\y.[\z.[y]]]|};;    (*\x.\y.\z.[y]*)
let t3 = string_to_lambda {|\x.[\y.[\z.[z]]]|};;    (*\x.\y.\z.[z]*)

let triplet_cons = string_to_lambda {|\x.[\y.[\z.[\t.[1(3(5(t)5*6(x)6)3*4(y)4)1*2(z)2]]]]|};;   (*\x.\y.\z.\t.[txyz]*)
let triplet1 = cbn (App(App(App(triplet_cons,V "a"),V "b"),V "c") ) ;;  (* (\x.\y.\z.\t.[txyz])abc = \t.[tabc]*)
let triplet2 = cbn (App(App(App(triplet_cons,V "d"),V "e"),V "f") ) ;;  (* (\x.\y.\z.\t.[txyz])def = \t.[tdef]*)

let proj = string_to_lambda {|\t.[\p.[1(p)1 * 2(t)2]]|};;   (*\t.\p.[pt]*)
let proj1 = cbn (App(proj,t1));;    (*proj1 : lamexp = Lam ("p", App (V "p", Lam ("x", Lam ("y", Lam ("z", V "x")))))*)
let proj2 = cbn (App(proj,t2));;    (*proj2 : lamexp = Lam ("p", App (V "p", Lam ("x", Lam ("y", Lam ("z", V "y")))))*)
let proj3 = cbn (App(proj,t3));;    (*proj3 : lamexp = Lam ("p", App (V "p", Lam ("x", Lam ("y", Lam ("z", V "z")))))*)

let result11 = cbn (App(proj1,triplet1));; (*output = result11 : lamexp = V "a"*)
let result12 = cbn (App(proj2,triplet1));; (*output = result12 : lamexp = V "b"*)
let result13 = cbn (App(proj3,triplet1));; (*output = result13 : lamexp = V "c"*)
let result21 = cbn (App(proj1,triplet2));; (*output = result21 : lamexp = V "d"*)
let result22 = cbn (App(proj2,triplet2));; (*output = result22 : lamexp = V "e"*)
let result23 = cbn (App(proj3,triplet2));; (*output = result23 : lamexp = V "f"*)

let d = string_to_lambda {|\t.[\x.[\y.[\z.[1(3(5(t)5*6(x)6)3*4(y)4)1*2(z)2]]]]|} ;; (*conditional*)
let result1 = cbn (App(App(App(App (d,t1),V "u"),V "v"),V "w")) ;;    (*DT1uvw = u*)
let result2 = cbn (App(App(App(App (d,t2),V "u"),V "v"),V "w")) ;;    (*DT2uvw = v*)
let result3 = cbn (App(App(App(App (d,t3),V "u"),V "v"),V "w")) ;;    (*DT3uvw = w*)

(*============test case 3: church numerals ========*)
let zero = string_to_lambda {|\f.[\x.[x]]|};;   (*zero = \f.\x.[x]*)
let one = string_to_lambda {|\f.[\x.[1(f)1*2(x)2]]|};;  (*one = \f.\x.[fx]*)
let sum = string_to_lambda {|\m.[\n.[\f.[\x.[1(3(m)3*4(f)4)1*2(5(7(n)7*8(f)8)5*6(x)6)2]]]]|};; (*sum = \m.\n.\f.\x.[(mf)(nfx)]*)
let two = cbn ( App (App(App(App(sum,one),one),V "g"),V "z") );;
match two with App(e1,e2) -> App(e1,cbn e2);;   (*output = - : lamexp = App (V "g", App (V "g", V "z"))*)
lambda_to_string two;; (*output = - : string = "( g ) * ( ( ( \\f.[ \\x.[ ( f ) * ( x ) ] ] ) * ( g ) ) * ( z ) )"*) (* i.e. g(one g z) *)
(* ========test_Case 3==========*)
(*weakly normalizing term, or, infinite reduction sequence*)
(*cbn oMega;;*) (*output = Stack overflow during evaluation (looping recursion?). *)




(* ==============TA Test cases================*)

(*Test case 1*)
let id = Lam ("x", V "x");;

let result = cbn id;;

Printf.printf "Identity Function: %s\n"
  (match result with
   | Lam (x, V y) when x = y -> "\\x.x"
   | _ -> "Failure");;



(*Test case 2*)
let id_app = App (Lam ("x", V "x"), V "y");;

let result = cbn id_app;;

Printf.printf "Application of Identity Function: %s\n"
  (match result with V x -> x | _ -> "Failure");;


(*Test case 3*)
let nested = Lam ("x", Lam ("y", V "x"));;

let result = cbn nested;;

Printf.printf "Nested Abstractions: %s\n"
  (match result with
   | Lam (x, Lam (y, V z)) when z = x -> "\\x.\\y.x"
   | _ -> "Failure");;


(*Test case 4*)
let pair = Lam ("x", Lam ("y", Lam ("z", App (App (V "z", V "x"), V "y"))));;
let first = Lam ("p", App (V "p", Lam ("x", Lam ("y", V "x"))));;
let second = Lam ("p", App (V "p", Lam ("x", Lam ("y", V "y"))));;

let pair_app = App (App (pair, V "a"), V "b");;
let first_app = App (first, pair_app);;
let second_app = App (second, pair_app);;

let result_first = cbn first_app;;
let result_second = cbn second_app;;


Printf.printf "First: %s\nSecond: %s\n"
  (match result_first with V x -> x | _ -> "Failure")
  (match result_second with V x -> x | _ -> "Failure");;


(*Test case 5*)
let term = App (Lam ("x", Lam ("y", V "x")), V "y");;


let result = cbn term;;

Printf.printf "Substitution Avoiding Variable Capture: %s\n"
  (match result with
   | Lam (x, Lam (y, V z)) when z = x -> "\\x.\\y.x"
   | _ -> "Failure");;


Printf.printf "Substitution Avoiding Variable Capture: %s\n"
  (match result with
   | Lam (z,  V y) when (z="z"&&y="y") -> "\\z.y"
   | _ -> "Failure");;
