type expb = T | F | And of expb*expb | Or of expb*expb | Not of expb | Implies of expb*expb | Iff of expb*expb;;


let rec evalb e : bool = match e with(*Definitional interpreter of AST expb*)
    T -> true
  | F -> false
  | And(e1,e2) -> (evalb e1) && (evalb e2)
  | Or(e1,e2) -> (evalb e1) || (evalb e2)
  | Not e1 -> not (evalb e1)
  | Implies(e1,e2) -> let a,b = (evalb e1),(evalb e2) in
      (match (a,b) with (true,false) -> false | _ -> true)
  | Iff(e1,e2) -> let a,b = (evalb e1),(evalb e2) in
      (match (a,b) with (true,true) -> true | (false,false) -> true | _ -> false) 
;;

(* Some functions characterizing AST evalb*)
let rec height e (h:int) : int = match e with  
    T -> h+1   (*Leaf nodes are at height=1*)
  | F -> h+1   (*Leaf nodes are at height=1*)
  | And(e1,e2) -> max (height e1 h+1) (height e2 h+1)
  | Or(e1,e2) -> max (height e1 h+1) (height e2 h+1)
  | Not e1 -> height e1 h+1
  | Implies(e1,e2) -> max (height e1 h+1) (height e2 h+1)
  | Iff(e1,e2) -> max (height e1 h+1) (height e2 h+1)
;;
let rec size e (s:int) : int = match e with
    T -> s+1
  | F -> s+1
  | And(e1,e2) -> size e2 (size e1 s+1)
  | Or(e1,e2) -> size e2 (size e1 s+1)
  | Implies(e1,e2) -> size e2 (size e1 s+1)
  | Iff(e1,e2) -> size e2 (size e1 s+1)
  | Not e1 -> size e1 s+1
;;



type opcodeb = LD of bool | AND | OR | NOT | IMPLIES | IFF;;

let rec compileb e : opcodeb list = match e with (*compiler for AST expb*)
    T -> [LD true]
  | F -> [LD false]
  | And(e1,e2) -> (compileb e1) @ (compileb e2) @ [AND]
  | Or(e1,e2) -> (compileb e1) @ (compileb e2) @ [OR]
  | Not e1 -> (compileb e1) @ [NOT]
  | Implies(e1,e2) -> (compileb e1) @ (compileb e2) @ [IMPLIES]
  | Iff(e1,e2) -> (compileb e1) @ (compileb e2) @ [IFF]
;;

exception Stuck;;
let rec stkmcb (s:bool list) (ops:opcodeb list) : bool list = match ops with (*stack machine implementing opcodeb, if stuck raises exception'Stuck'*)
    [] -> s
  | (LD b)::rest -> stkmcb (b::s) rest
  | OR::rest -> (match s with b::(a::rest_) -> (stkmcb ( (a || b)::rest_) rest) | _ -> raise Stuck )
  | AND::rest -> (match s with b::(a::rest_) -> (stkmcb ( (a && b)::rest_) rest) | _ -> raise Stuck )
  | NOT::rest -> ( match s with a::rest_ -> (stkmcb ( (not a)::rest_) rest) | _ -> raise Stuck )
  | IMPLIES::rest -> 
      ( match s with 
          b::(a::rest_) -> 
            ( match (a,b) with (true,false) -> stkmcb ( false::rest_) rest
                             | _ -> stkmcb ( true::rest_) rest
            )
        | _ -> raise Stuck
      )
  | IFF::rest -> 
      (match s with 
         b::(a::rest_) -> 
           (match (a,b) with 
              (true,true) -> stkmcb ( true::rest_) rest
            | (false,false) -> stkmcb ( true::rest_) rest 
            | _ -> stkmcb ( false::rest_) rest
           )
       | _ -> raise Stuck
      )
;;
(* Lemma 1:
   For all op1:opcodeb list,for all op2:opcodeb list, for all s:bool list,
         If (stkmcb s op1) don't raise 'Stuck',then (stkmcb s op1@op2 = stkmcb (stkmcb s op1) op2 ).
   proof : by Induction on size of op1, for all op2:opcodeb list, for all s:bool list
     Base case: size(op1) = 1. Then, op1 can be [LD true] or [LD false]. In this case, (stkmcb s op1) don't raise 'Stuck'
       when op1=[LD true]:
         stkmcb s op1@op2 = stkmcb s [LD true]@op2 = stkmcb (true::s) op2 = stkmcb (stkmcb s [LD true]) op2 = stkmcb (stkmcb s op1) op2
       when op1=[LD false]:
         stkmcb s op1@op2 = stkmcb s [LD false]@op2 = stkmcb (false::s) op2 = stkmcb (stkmcb s [LD false]) op2 = stkmcb (stkmcb s op1) op2
     Inductive case: For all op1:opcodeb list with size(op1)=k,for all op2:opcodeb list,for all s:bool list
                       If (stkmcb s op1) don't raise 'Stuck',then (stkmcb s op1@op2 = stkmcb (stkmcb s op1) op2 ).
       Consider case when size(op1) = k+1.
       then, op1 is of form op1_1@rest where size(op1_1)=1 and size(rest) = k.
       For all op2:opcode list
         op1@op2 = op1_1@(rest@op2), and 
       Let s:bool list be such that (stkmcb s op1) don't raise 'Stuck'. => (stkmcb s op1_1) don't raise 'Stuck' (since stkmcb works on 1 opcodeb at a time,so would raise 'Stuck' immediately),
   and by base case, stkmcb s op1 = stkmcb s op1_1@rest = (stkmcb (stkmcb s op1_1) rest) also don't raise 'Stuck'.
       So, stkmcb s op1@op2 = stkmcb s op1_1@(rest@op2)
            = stkmcb (stkmcb s op1_1) rest@op2  # by base case, where second opcodeb list is (rest@op2) #
            = stkmcb (stkmcb (stkmcb s op1_1) rest) op2   # by induction assumption#
            = stkmcb ( stkmcb s op1_1@rest ) op2  # by base case #
            = stkmcb ( stkmcb s op1) op2
    Thus, proved the claim.
*)
(* Lemma 2:
   for all e:expb, for all b:bool, ( stkmcb [] (compileb e) = [b] ) if and only if (for all s:bool list, stkmcb s (compileb e) =b::s)
   proof : 
     claim L2.1: ( <= ) for all e:expb,for all b:bool, if (for all  s:bool list, stkmcb s (compileb e) = b::s) then (stkmcb [] (compileb e) = [b]).
     proof : Let e:expb and b:bool be such that, (for all  s:bool list, stkmcb s (compileb e) = b::s) is true. 
                Then, Since [] is a bool list, for s=[], stkmcb [] (compileb e) = b::[] = [b].
   
     claim L2.2: ( => ) for all e:expb,for all b:bool, if (stkmcb [] (compileb e) = [b] ) then (for all s:bool list, stkmcb s (compileb e) = s::b).
     proof : by induction on height(e).
      Base case: for all e:expb such that height(e)=1. Thus, only two possibilities for e i.e. 'T' or 'F'.
       when e=T:
         stkmcb [] (compileb T) = stkmcb [] [LD true] = stkmcb [true] [] = [true].
         Thus, stkmcb [] (compileb T) = [b] is true for b = true only.
         Then, for b=true, for all s:bool list
           stkmcb s (compileb T) = stkmcb s [LD true] = stkmcb true::s [] = true::s = b::s.
       when e=F:
         stkmcb [] (compileb F) = stkmcb [] [LD false] = stkmcb [false] [] = [false].
         Thus, stkmcb [] (compileb F) = [b] is true for b = false only.
         Then, for b=false, for all s:bool list
           stkmcb s (compileb F) = stkmcb s [LD false] = stkmcb false::s [] = false::s = b::s.
      Inductive case: Let for all e:expb such that height(e)<=k, for all b:bool, 
                              if ( stkmcb [] (compileb e) = [b] ) then ( for all s:bool list,stkmcb s (compileb e) = b::s ).
       Consider e:expb such that height(e)=k+1,
   
         When e=Not e1, where e1:expb is such that height(e1)<=k,
          Let b:bool be such that stkmcb [] (compileb e) = [b].
           Then, b = (not b1), where b1:bool is such that (stkmcb [] (compileb e1)=[b1]).
           Now,
            for all s:bool list, stkmcb s (compileb Not e1) = stkmcb [] (compileb e1)@[NOT]
             = stkmcb (stkmcb s (compileb e1)) [NOT]  # by Lemma 1 #
             = stkmcb b1::s [NOT]  # by Induction assumption #
             = stkmcb (not b1)::s []
             = (not b1)::s = b::s
  
         when e=Op(e1,e2), where e1,e2:expb are such that height(e1)<=k and height(e2)<=k,
          Let b:bool be such that stkmcb [] (compileb e) = [b].
           Then, b = (b1 op b2), where b1,b2:bools are such that (stkmcb [] (compileb e1)=[b1]) and (stkmcb [] (compileb e1)=[b1]).
           Now,
            for all s:bool list, stkmcb s (compileb Op(e1,e2)) = stkmcb [] (compileb e1)@(compileb e2)@[OP]
             = stkmcb ( stkmcb (stkmcb s (compileb e1)) (compileb e2) ) [OP]  # by Lemma 1 #
             = stkmcb ( stkmcb b1::s (compile e2) ) [OP]  # by Induction assumption #
             = stkmcb b2::(b1::s) [OP]  # by Induction assumption #
             = stkmcb (b1 op b2)::s [] = (b1 op b2)::s = b::s
         
          for all ('Op','OP','op') in { ('And','AND','&&'), ('Or','OR','||'), ('Implies','IMPLIES','implies'), ('Iff','IFF','iff') }.
         

   
      Q.E.D

  
*)


let calculateb e : bool = match (stkmcb [] (compileb e) ) with [b] -> b | _ -> raise Stuck;;


(* ====================== THEOREMS ============= *)
(* Theorem 1:
   Correctness of the compiler 'compileb' and execution of the stack machine 'stkmcb' wrt the standard
   reference semantics given by the definitional interpreter 'evalb' i.e.
   for all e: expb, calculateb e = evalb e .
  
  Proof:by Induction on height(e).  # height() is defined earlier #
     Base case: height(e) = 1. Thus, possible e are 'T' and 'F'.
        Consider when e=T: 
          stkmcb [] (compileb T) = stkmcb [] (LD true) = stkmcb [true] [] = [true]
         Then,
          calculateb T = true = evalb T
        Consider when e=F:
          stkmcb [] (compileb F) = stkmcb [] (LD false) = stkmcb [false] [] = [false]
         Then,
          calculateb F = false = evalb F
     Inductive case: Let for all e in expb with height(e) <= k where k>=2 is integer, calculateb e = evalb e
   
        Consider when height(e) = k+1. Then, possible e are:
           e=Not e1 where height(e1)<=k.
             By Induction hypothesis, calculate e1 = evalb e1 => stkmcb [] (compileb e1) = [evalb e1]
   
             stkmcb [] (compile e) = stkmcb [] ((compileb e1)@[NOT]) 
             = stkmcb ( stkmcb [] (compileb e1) ) [NOT]  # Using lemma:1 of stkmcb #
             = stkmcb [evalb e1] [NOT]  # Using Induction Assumption,  #
             = [not (evalb e1)] = [evalb (Not e1)] = [evalb e]
             Thus, calculateb e = evalb e 
  
           e=Op(e1,e2) where height(e1)<=k and height(e2)<=k.
             By Induction hypothesis, calculateb e1 = evalb e1 => stkmcb [] (compileb e1) = [evalb e1]
                                  and calculateb e2 = evalb e2 => stkmcb [] (compileb e2) = [evalb e2]
             stkmcb [] (compile e) = stkmcb [] ((compileb e1)@(compileb e2)@[OP]) 
             = stkmcb ( stkmcb ( stkmcb [] (compileb e1) ) (compileb e2) ) [OP]  # Using lemma:1 of stkmcb #
             = stkmcb ( stkmcb [evalb e1] (compileb e2) ) [OP]  # using Induction assumption for e1# 
             = stkmcb [evalb e2 ; evalb e1] [OP]  # Using Induction Hypothesis for e2 and lemma:2 of stkmcb#
             = [(evalb e1) op (evalb e2)] = [evalb (Op(e1,e2))] = [evalb e]
             Thus, calculateb e = evalb e
             
             for all ('Op','OP','op') in { ('And','AND','&&'), ('Or','OR','||'), ('Implies','IMPLIES','implies'), ('Iff','IFF','iff') }.

      
      Q.E.D 
           
     
*)
(* Theorem 2:
   Soundness of the “implementation”, i.e.
     for all e: expb, for all b: bool, If stkmcb [] (compileb e)= [b] then evalb e = b.
   
  Proof:by induction on height(e).   # height() is defined earlier #

   Base case: for all e in expb such that height(e) = 1. Possible e are 'T' or 'F'.
     when e=T:
       stkmcb [] (compileb T)= stkmcb [] [LD true] = stkmcb true::[] [] = [true].
       So, stkmcb [] (compileb T) = [b] is true for only b=true, and for this case, then evalb T = true = b.
       Thus, If stkmcb [] (compileb T) = [b], then evalb T = b.
     when e=F:
       stkmcb [] (compileb F)= stkmcb [] [LD false] = stkmcb false::[] [] = [false].
       So, stkmcb [] (compileb F) = [b] is true for only b=false, and for this case, then evalb F = false = b.
       Thus, If stkmcb [] (compileb F) = [b], then evalb F = b.
   
   Inductive case: Let for all e in expb with height(e)<=k (where k>2 is int), for all b:bool, If ( stkmcb [] (compileb e)=[b] ) then evalb e=b.
     Now, consider for all e in expb with height(e) = k+1,
   
     when e=Not e1, where e1 is some expb such that height(e1) <= k.
     Let b:bool be such that ( stkmcb [] (compileb e) = [b] ), then (b = not b1) where b1:bool is such that (stkmcb [] (compileb e1) = [b1]).
         stkmcb [] (compileb e) = stkmcb [] (compileb e1)@[NOT]
         = stkmcb (stkmcb [] (compileb e1) ) [NOT]   # Using lemma:1 of stkmcb#
         = stkmcb b1::[] [NOT]  # Using Induction assumption #
         = (not b1)::[] = [b]
     and evalb (Not e1) = not (evalb e1) = not b1 = b. # Using Induction assumption, evalb e1 = b1#
     
     when e=Op(e1,e2) where e1,e2 are in expb and height(e1)<=k and height(e2)<=k.
     Let b:bool be such that ( stkmcb [] (compileb e) = [b] ),
       then (b = b1 op b2) where b1,b2:bool are such that (stkmcb [] (compileb e1) = [b1]) and (stkmcb [] (compileb e2) = [b2])
     Consider,
         stkmcb [] (compileb e) = stkmcb [] (compileb e1)@(compileb e2)@[OP]  
         = stkmcb ( stkmcb (stkmcb [] (compileb e1) ) (compileb e2) ) [OP]   # using lemma:1 of stkmcb#
         = stkmcb b2::(b1::[]) [OP]   # using Induction Hypothesis, and lemma:2 of stkmcb#
         = stkmcb (b1 op b2)::[] [] = [b1 op b2] = [b]
     and evalb Op(e1,e2) = (evalb e1) op (evalb e2) = b1 op b2 = b.   # Using Induction assumption, (evalb e1 = b1) and (evalb e2 = b2) #
   
      for all ('Op','OP','op') in { ('And','AND','&&'), ('Or','OR','||'), ('Implies','IMPLIES','implies'), ('Iff','IFF','iff') }.

  Thus, for all e:expb,for all b:bool, if stkmcb [] (compileb e)= [b], then evalb e = b.
   
     Q.E.D.
       
*)
(* Theorem 3:
  Completeness of the “implementation”, i.e.
     for all e: expb, for all b: bool, If evalb e = b then stkmcb [] (compileb e)= [b]
   is true.
  
  proof: by Induction on height(e).
   Base case: for all e in expb such that height(e) = 1. Then possible e are 'T' and 'F'.
     When e=T: Let b:bool be such that, evalb e = b. Then,
       evalb e = evalb T = true implies b=true.
       Consider,
         stkmcb [] (compileb e) = stkmcb [] (compileb T) = stkmcb [] [LD true] = stkmcb true::[] [] = [true]
          = [b]

     When e=F: Let b:bool be such that, evalb e = b. Then,
       evalb e = evalb F = false implies b=false.
       Consider,
         stkmcb [] (compileb e) = stkmcb [] (compileb F) = stkmcb [] [LD false] = stkmcb false::[] [] = [false]
          = [b]
      
   Indutive case: Let for all e in expb with height(e)<=k where k>1 is integer, for all b:bool
                             if evalb e = b, then stkmcb [] (compileb e) = [b].
    Consider for all e in expb with height(e)=k+1,
   
     When e=Not e1: where e1 in expb and height(e1)<=k.
      Let b:bool be such that (evalb e = b). Then, (b = not b1) where b1:bool is such that (evalb e1 = b1).
      Consider
         stkmcb [] (compileb e) = stkmcb [] (compileb (Not e1)) = stkmcb [] (compileb e1)@[NOT]
        = stkmcb (stkmcb [] (compileb e1)) [NOT]  # using lemma1 #
        = stkmcb [b1] [NOT]  # using Induction assumption #
        = stkmcb (not b1)::[] [] = [not b1] = [b]
      
     When e=Op(e1,e2): where e1,e2 in expb and height(e1)<=k and height(e2)<=k.
      Let b:bool be such that (evalb e = b). Then, (b= b1 op b2) where b1,b2:bools are such that (evalb e1 = b1) and (evalb e2 = b2).
      Consider,
         stkmcb [] (compileb e) = stkmcb [] (compileb Op(e1,e2) ) = stkmcb [] (compileb e1)@(compileb e2)@[OP]
         = stkmcb ( stkmcb ( stkmcb [] (compileb e1) ) (compileb e2 ) [OP]  # by lemma 1 of stkmcb#
         = stkmcb b2::(b1::[]) [OP]  # Using Induction assumption and lemma:2 of stkmcb#
         = stkmcb (b1 op b2)::[] [] = [b1 op b2] = [b]
   
      for all ('Op','OP','op') in { ('And','AND','&&'), ('Or','OR','||'), ('Implies','IMPLIES','implies'), ('Iff','IFF','iff') }.

   Thus, for all e:expb,for all b:bool, If evalb e = b, then stkmcb [] (compileb e) = [b]. 
   Q.E.D.

*) 

(* ==================== TEST CASES ======================*)
let e1:expb = And(Or(T,F),Iff(T,F));;   (* (true + false).(T <==> F) = false*)
let e2:expb = Or(And(T,Not T),And(Not T,T));;   (* true Xor true = false  *)
let e3:expb = Implies(F,T);;  (* (false => true) = true*)
let e4:expb = Implies(T,F);;  (* (true => false) = false*)
let e5:expb = Iff(F,T);;  (* (false <==> true) = false*)
                          
height e1 0;;  (*output= - : int = 3*)
height e2 0;;  (*output= - : int = 4*)
height e3 0;;  (*output= - : int = 2*)
height e4 0;;  (*output= - : int = 2*)
height e5 0;;  (*output= - : int = 2*)
               
size e1 0;;  (*output= - : int = 7*) 
size e2 0;;  (*output= - : int = 9*)
size e3 0;;  (*output= - : int = 3*)
size e4 0;;  (*output= - : int = 3*)
size e5 0;;  (*output= - : int = 3*)
                                              
evalb e1;;  (*output= - : bool = false*)
evalb e2;;  (*output= - : bool = false*)
evalb e3;;  (*output= - : bool = true*)
evalb e4;;  (*output= - : bool = false*)
evalb e5;;  (*output= - : bool = false*)
            
evalb T;;  (*output= - : bool = true*)
evalb F;;  (*output= - : bool = false*)

evalb (Or(F,F));;  (*output= - : bool = false*)
evalb (Or(F,T));;  (*output= - : bool = true*)
evalb (Or(T,F));;  (*output= - : bool = true*)
evalb (Or(T,T));;  (*output= - : bool = ftrue*)

evalb (And(F,F));;  (*output= - : bool = false*)
evalb (And(F,T));;  (*output= - : bool = false*)
evalb (And(T,F));;  (*output= - : bool = false*)
evalb (And(T,T));;  (*output= - : bool = true*)

evalb (Not T);;  (*output= - : bool = false*)
evalb (Not F);;  (*output= - : bool = true*)

evalb (Implies(F,F));;  (*output= - : bool = true*)
evalb (Implies(F,T));;  (*output= - : bool = true*)
evalb (Implies(T,F));;  (*output= - : bool = false*)
evalb (Implies(T,T));;  (*output= - : bool = true*)

evalb (Iff(F,F));;  (*output= - : bool = true*)
evalb (Iff(F,T));;  (*output= - : bool = false*)
evalb (Iff(T,F));;  (*output= - : bool = false*)
evalb (Iff(T,T));;  (*output= - : bool = true*)
                    
let op1 = compileb e1;;  (*output= val op1 : opcodeb list = [LD true; LD false; OR; LD true; LD false; IFF; AND]*)
let op2 = compileb e2;;  (*output= val op2 : opcodeb list = [LD true; LD true; NOT; AND; LD true; NOT; LD true; AND; OR]*) 
let op3 = compileb e3;;  (*output= val op3 : opcodeb list = [LD false; LD true; IMPLIES]*)
let op4 = compileb e4;;  (*output= val op4 : opcodeb list = [LD true; LD false; IMPLIES]*) 
let op5 = compileb e5;;  (*output= val op5 : opcodeb list = [LD false; LD true; IFF]*)
               
stkmcb [] op1;;  (*output= - : bool list = [false]*)
stkmcb [] op2;;  (*output= - : bool list = [false]*)
stkmcb [] op3;;  (*output= - : bool list = [true]*)
stkmcb [] op4;;  (*output= - : bool list = [false]*)
stkmcb [] op5;;  (*output= - : bool list = [false]*)
                 
calculateb e1 = evalb e1;;  (*output= - : bool = true*)
calculateb e2 = evalb e2;;  (*output= - : bool = true*)
calculateb e3 = evalb e3;;  (*output= - : bool = true*)
calculateb e4 = evalb e4;;  (*output= - : bool = true*)
calculateb e5 = evalb e5;;  (*output= - : bool = true*)