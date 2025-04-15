(*Structure of this file:
  -> code
  -> Test cases
*)

open List;;

type symbol = string*int;;

type signature = symbol list;;

type tree = V of string | C of {node:symbol;children:tree list};;

let check_sig (s:signature) : bool =  (*checks for symbol that they don't appear twice(either with same arity or different) and arity is non-negative integer*)
    let rec count (sym,arity) s = (fold_left (+) 0 (map (fun (str,ar) -> if str=sym then 1 else 0) s)
                                    ) in
    fold_left (&&) true ( map (fun (str,ar) -> (ar>=0) && (count (str,ar) s = 1)
                                ) s
                        )
;;

let rec wftree (t:tree) : bool = match t with
    V x -> if (x="") then false else true (*unempty var_name*)
  | C r -> match r.node with (sym,arity) -> if (length r.children = arity) then (fold_left (&&) true (map wftree r.children)) else false
;;


let ht (t:tree) : int =
  let rec ht_tail (d:int) (t_:tree) : int =
  (
    match t_ with
    V s -> d+1
  | C r -> fold_left (fun a b -> max a (ht_tail (d+1) b)) (d+1) r.children
  )
  in ht_tail 0 t
;;

let rec size (t:tree) : int = match t with
    V s -> 1
  | C r -> fold_left (+) 1 (map size r.children)
;;

let vars (t:tree) : string list =
    let rec vars_tail (v:string list) (t_:tree) =
    (
    match t_ with
        V x -> x::v
      | C r ->(
              let temp_fun = (fold_left (fun (x:(tree->string list)) y -> vars_tail (x y) ) (vars_tail v) r.children)
              in (match (temp_fun (V "temp")) with head::tail -> tail | _ -> [] )
              )
    ) in vars_tail [] t
;;



let rec mirror (t:tree) : tree = match t with
    C r -> C {node=r.node;children=(map (mirror) (rev r.children))}
  | _ -> t
;;

type substitution = (string*tree) list;;
let valid_substitution (sub:substitution) : bool =
    let count (name,_) s = (
                              fold_left (+) 0 (  (*Count the number of times var named 'name' appeared*)
                                              map (fun (n,v) -> if name=n then 1 else 0) s
                                              )
                             ) in
    fold_left (&&) true (  (*Checks if every variable appears once and no identity substitution*)
                          map (
                                fun (name,value) -> (not (value=V name)) && (count (name,value) sub = 1)
                              ) sub
                        )
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
(*============== TEST CASES ==============*)

check_sig [("a",2)];;   (*output = - : bool = true*)
check_sig [("a",2);("a",3)];;  (*output = - : bool = false*)
check_sig [("a",2);("b",3)];;   (*output = - : bool = true *)
check_sig [("a",2);("b",3);("v",-1)];; (*output = - : bool = false*)
check_sig [("a",2);("b",3);("v",0)];;  (*output = - : bool = true *)

wftree (C({node=("a",3);children=[V "c"]}));; (*output = - : bool = false *)
wftree (C({node=("a",1);children=[V "c"]}));; (*output = - : bool = true *)
wftree (C({node=("a",1);children=[V ""]}));; (*output = - : bool = false *)
wftree (C({node=("a",1);children=[C {node=("b",1);children=[]}]}));; (*output = - : bool = false *)

ht (V "ss");; (*output = - : int = 1 *)
(*Consider the tree:     "a"
                      /   |    \
            Var("f")     "d"     "h"
                          |
                         "g"
                          |
                        Var("k")
*)
ht (C {node=("a",3);children=[V "f";C {node=("d",1);children=[C {node=("g",1);children=[V "k"]}]};C {node=("h",0);children=[]}]});;  (*output = - : int = 4 *)
size (C {node=("a",3);children=[V "f";C {node=("d",1);children=[C {node=("g",1);children=[V "k"]}]};C {node=("h",0);children=[]}]});;  (*output = - : int = 6 *)

vars (V "x");; (*output = - : string list = ["x"]*)
vars (C {node=("a",0);children=[]});;  (*output = - : string list = []*)
vars (C {node=("a",3);children=[V "f";C {node=("d",1);children=[C {node=("g",1);children=[V "k"]}]};C {node=("h",0);children=[]}]});; (*output = - : string list = ["k"; "f"]*)

mirror (C {node=("a",3);children=[V "f";C {node=("d",1);children=[C {node=("g",1);children=[V "k"]};V "o"]};C {node=("h",0);children=[]}]});;
(*output = - : tree =
C
 {node = ("a", 3);
  children =
   [C {node = ("h", 0); children = []};
    C
     {node = ("d", 1);
      children = [V "o"; C {node = ("g", 1); children = [V "k"]}]};
    V "f"]}

*)


valid_substitution [];;  (*output = - : bool = true*)
valid_substitution [("x",V "x")];;  (*output = - : bool = false*)   (*substitution should include vars where it's not identity*)
valid_substitution [("x",C {node=("a",0);children=[]});("x",V "y")];;  (*output = - : bool = false*)  (*same var is substituted for different trees*)
valid_substitution [("x",C {node=("a",0);children=[]});("x",C {node=("a",0);children=[]})];;  (*output = - : bool = false*)  (*same substitution listed twice*)
valid_substitution [("x",C {node=("a",0);children=[]});("y",V "l")];;  (*output = - : bool = true*)

subst [] (C {node=("a",1);children=[V "x"]});;  (*output = - : tree = C {node = ("a", 1); children = [V "x"]}*)
subst [("x",C {node=("b",0);children=[]})] (C {node=("a",1);children=[V "x"]});;  (*output = - : tree = C {node = ("a", 1); children = [C {node=("b",0);children=[]}]} *)
subst [("x",V "y");("y",V "x")] (C {node=("a",1);children=[V "x"]});; (*output = - : tree = C {node = ("a", 1); children = [V "y"]}*)

compose_subst [] [];;  (*output = - : substitution = []*)
compose_subst [] [("x",V "y")];; (*output = - : substitution = [("x",V "y")]*)
compose_subst [("x",V "y")] [("y",V "x")];;  (*output = - : substitution = [("y", V "x")]*)
compose_subst [("x",C {node=("a",1);children=[V "x"]});("y",V "x")] [("z",V "x");("x",V "y");("y",V "x")];;
(*output = [("x", C {node = ("a", 1); children = [V "y"]}); ("z", V "x")] *)


(*=====mgu test cases====*)
(*when both trees are variables*)
mgu (V "x") (V "y");;  (*output = - : substitution = [("x", V "y")]*)



(*when one tree is a variable and other tree contain that variable*)
try (mgu (V "x") (C {node=("a",2);children=[V "x";C {node=("b",1);children=[V "z"]}]}) ) with NOT_UNIFIABLE -> [("NOT_UNIFIABLE",V "NA")];;
(*output = - : substitution = [("NOT_UNIFIABLE", V "NA")]*)



(*when one tree is variable and other tree does not contain this variable and is not a variable itself*)
mgu (C {node=("a",2);children=[V "z";C {node=("b",1);children=[V "y"]}]}) (V "x");;
(*output = - : substitution =
[("x",
  C
   {node = ("a", 2);
    children = [V "z"; C {node = ("b", 1); children = [V "y"]}]})]
*)



(*when both trees are not variable and root node of both are different*)
try mgu (C {node=("a",0);children=[]}) (C {node=("b",1);children=[V "z"]}) with NOT_UNIFIABLE->[("NOT_UNIFIABLE",V "NA")];;
(*output = - : substitution = [("NOT_UNIFIABLE", V "NA")]*)



(*when both trees are not variable, but have same root node. Yet can't be unified*)
(*TREE1:    "a"
          /     \
      Var("z")    "b"
                    \
                    Var("y")
*)

(*TREE2:    "a"
          /     \
        "a"       Var("y")
      /     \
  Var("r")    Var("p")
*)
(*analysis: subtree of TREE1 rooted at "b" can't be unified with subtree of TREE2 rooted at Var("y"), since the former includes the latter and it remains so even after substitution involved in algorithm.*)
try mgu (C {node=("a",2);children=[V "z";C {node=("b",1);children=[V "y"]}]}) (C {node=("a",2);children = [C {node=("a",2);children=[V "r";V "p"]};V "y"]})
with
NOT_UNIFIABLE->[("NOT_UNIFIABLE",V "NA")];; (*output = - : substitution = [("NOT_UNIFIABLE", V "NA")]*)



(*when both trees are not variables and can be unified. *)
(*TREE1:        "a"
            /         \
          Var("z")    "b"
                         \
                         Var("y")
*)

(*TREE2:        "a"
              /      \
            "a"       Var("k")
          /     \
    Var("r")    Var("p")
*)
mgu (C {node=("a",2);children=[V "z";C {node=("b",1);children=[V "y"]}]}) (C {node=("a",2);children = [C {node=("a",2);children=[V "r";V "p"]};V "k"]});;
(*output = - : substitution =
[("z", C {node = ("a", 2); children = [V "r"; V "p"]});
 ("k", C {node = ("b", 1); children = [V "y"]})]
*)



(*when both trees are not variables, but have same root node. Yet can't be unified*)
(*TREE1:      "a"
            /     \
      Var("z")      "b"
                  /     \
              Var("z")    Var("p")
*)

(*TREE2:      "a"
            /     \
          "a"     Var("k")
        /     \
  Var("r")      Var("k")
*)
(*Analysis: when Var("z") in subtree rooted at "b"(of TREE1) is substituted, then resulting subtree can't be unified with subtree rooted at Var("k") of (TREE2:level2)*)
try (mgu (C {node=("a",2);children=[V "z";C {node=("b",2);children=[V "z";V "p"]}]}) (C {node=("a",2);children = [C {node=("a",2);children=[V "r";V "k"]};V "k"]})) with
NOT_UNIFIABLE->[("NOT_UNIFIABLE",V "NA")];; (*output = - : substitution = [("NOT_UNIFIABLE", V "NA")]*)



(*when both trees are not variables and can be unified*)
(*TREE1:        "a"
              /     \
          Var("z")   "b"
                    /    \
                Var("z")  Var("r")
*)
(*TREE2:        "a"
              /     \
            "a"       Var("k")
          /     \
      Var("p")   Var("p")
*)
mgu (C {node=("a",2);children=[V "z";C {node=("b",2);children=[V "z";V "r"]}]}) (C {node=("a",2);children = [C {node=("a",2);children=[V "p";V "p"]};V "k"]});;
(*output = - : substitution =
[("z", C {node = ("a", 2); children = [V "p"; V "p"]});
 ("k",
  C
   {node = ("b", 2);
    children = [C {node = ("a", 2); children = [V "p"; V "p"]}; V "r"]})]
*)



(*======= properties of mgu=====*)
subst_equality (mgu (C {node=("a",2);children=[V "z";C {node=("b",2);children=[V "z";V "r"]}]}) (C {node=("a",2);children = [C {node=("a",2);children=[V "p";V "p"]};V "k"]})
) (mgu (mirror(C {node=("a",2);children=[V "z";C {node=("b",2);children=[V "z";V "r"]}]})) (mirror(C {node=("a",2);children = [C {node=("a",2);children=[V "p";V "p"]};V "k"]}))
) ;;  (*output = - : bool = false*)  (*mgu t u =/= mgu (mirror t) (mirror u) ,

      instead,  mgu t u = substitution_mirror (mgu (mirror t) (mirror u)), as shown below *)


subst_equality (mgu (C {node=("a",2);children=[V "z";C {node=("b",1);children=[V "y"]}]}) (C {node=("a",2);children = [C {node=("a",2);children=[V "r";V "p"]};V "k"]})
) (substitution_mirror (mgu (mirror (C {node=("a",2);children=[V "z";C {node=("b",1);children=[V "y"]}]})) (mirror (C {node=("a",2);children = [C {node=("a",2);children=[V "r";V "p"]};V "k"]})))
);;  (*output = - : bool = true *)

subst_equality (mgu (C {node=("a",2);children=[V "z";C {node=("b",2);children=[V "z";V "r"]}]}) (C {node=("a",2);children = [C {node=("a",2);children=[V "p";V "p"]};V "k"]})
) (substitution_mirror(mgu (mirror(C {node=("a",2);children=[V "z";C {node=("b",2);children=[V "z";V "r"]}]})) (mirror(C {node=("a",2);children = [C {node=("a",2);children=[V "p";V "p"]};V "k"]}))
) ) ;;  (*output = - : bool = true *)



(*==================GIVEN TESTCASES=============*)
let sig1 = [("0", 0); ("1", 0); ("0", 1)];;
let sig2 = [("0", 0); ("1", 0); ("+", 2)];;
let t = C {node = ("+", 2); children = [(V "x"); (V "y"); (V "z")]} ;;
let t2 = C {node = ("+", 2); children = [(V "x"); (V "y")]} ;;
let t3 = C {node = ("+", 2); children = [(V "z"); t2]} ;;

check_sig sig1;;
wftree t;;
ht t2;;
size t2;;
mirror t3;;
