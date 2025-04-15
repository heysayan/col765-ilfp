(*=========Structure of file========*)
(*1. Code
  2. Test cases with corresponding output
*)

open List;;

type vector = float list;;
exception DimError;;

let rec zero n:vector = match n with
    0 -> []
  | _ -> 0.0::(zero (n-1));;


let dim v:int = length v;;



let opp v:vector = map (fun x -> -.x) v;;



let addv v1 v2 :vector = map (fun (x,y) -> x+.y ) 
    (try combine v1 v2 with Invalid_argument _-> (raise DimError));;
(*Lemma: addv is commutative
proof: by case analysis, consider arbitrary dimension d
for all vectors x,y of dimension d,
  addv x y = [(x1+.y1),...,(xd+.yd)] = [(y1+.x1.),...,(yd+.xd)]    #by commutativity of +.#
   = add y x
Q.E.D
*)


let subv v1 v2 :vector = map (fun (x,y) -> x-.y ) 
    (try combine v1 v2 with Invalid_argument _-> (raise DimError));; 
(*Lemma: addv is anticommutative
proof: by case analysis, consider arbitrary dimension d
for all vectors x,y of dimension d,
  subv x y = [(x1-.y1),...,(xd-.yd)] = opp([(y1-.x1.),...,(yd-.xd)])    #by anticommutativity of -.#
   = opp(subv y x)    # note: here (opp v) is vector with components as negated components of v #
Q.E.D
*)


let scalarmult f v:vector = map (fun x -> f*.x) v;;
(*Lemma: scalarmult is distributive over operands of addv and subv*)
(*proof: by case analysis, consider arbitrary dimension d
  for all vectors X,Y of dimension d and for all float values f,
  where X = [x1,...,xd] and Y = [y1,...,yd]
  scalarmult f (addv v1 v2) = scalarmult f [(x1+.y1),...,(xd+.yd)] = [f*.(x1+.y1),...,f*.(xd+.yd)]
  #using distributivity of *. between operands of +.#
   = [(f*.x1+f*.y1),...,(f*.xd+f*.yd)] = addv [f*.x1,...,f*.xd] [f*.y1,...,f*.yd] = addv (scalarmult f X) (scalarmult f Y)  
  
  similarly,
  scalarmult f (subv v1 v2) = scalarmult f [(x1-.y1),...,(xd-.yd)] = [f*.(x1-.y1),...,f*.(xd-.yd)]
  #using distributivity of *. between operands of -.#
   = [(f*.x1-f*.y1),...,(f*.xd-f*.yd)] = subv [f*.x1,...,f*.xd] [f*.y1,...,f*.yd] = subv (scalarmult f X) (scalarmult f Y)  
  
  Q.E.D
*)
(*Lemma: sequential scalarmult operations by two scalars are commutative*)
(*proof: by case analysis, consider arbitrary dimension d
  for all vectors X(=[x1,...,xd]) and float values f1,f2,
  scalarmult f1 (scalarmult f2 X) = scalarmult f1 [f2*.x1,...,f2*.xd] = [f1*.f2*.x1,...,f1*.f2*.xd]
   = [f2*.f1*.x1,...,f2*.f1*.xd]   # using commutativity of scalar operation *. #
  = scalarmult f2 [f1*.x1,...,f1*.xd] = scalarmult f2 (scalarmult f1 X)
  
  Q.E.D
*)


let rec dotprod v1 v2 :float = 
  fold_left (fun x->fun y-> x+.y) 0.0 (map (fun (x,y)-> x*.y) 
                                         (try combine v1 v2 with Invalid_argument _-> (raise DimError)) );;



let rec norm_squared v :float = fold_left (fun x->fun y->x+.y) 0.0 (map (fun x-> x*.x) v);;
let norm v :float = sqrt(norm_squared v);;



let rec normalise v : vector = let n = norm v in map (fun x->x/.n) v;;



let parallel v1 v2 : bool = let (n1,n2,d) = (norm v1,norm v2,dotprod v1 v2) in 
  if (n1*.n2 -. d < 0.000000001) then true else false;;



let rec crossprod v1 v2:vector = match v1 with
    [1.0;0.0;0.0] -> (match v2 with 
        [_;b2;b3] -> subv [0.0;0.0;b2] [0.0;b3;0.0]
      | _ -> (raise DimError)
    )
  | [0.0;1.0;0.0] -> (match v2 with
        [b1;_;b3] -> subv [b3;0.0;0.0] [0.0;0.0;b1]
      | _ -> (raise DimError)
    )
  | [0.0;0.0;1.0] -> (match v2 with
        [b1;b2;_] -> subv [0.0;b1;0.0] [b2;0.0;0.0]
      | _ -> (raise DimError)
    )
  | [a1;a2;a3] -> addv ( addv (scalarmult a1 (crossprod [1.0;0.0;0.0] v2) ) 
                           (scalarmult a2 (crossprod [0.0;1.0;0.0] v2) ) )
                    (scalarmult a3 (crossprod [0.0;0.0;1.0] v2) ) 
  | _ -> raise DimError
;; 

(** To prove : for all v1,v2 in R^3 , crossprod v1 v2 = opp (crossprod v2 v1)
   proof: (by case analysis)
   let A = [a1;a2;a3] and B = [b1;b2;b3] be arbitrary vectors in R^3.
   crossprod A B = ( a1*crossprod E1 B )  + ( a2*crossprod E2 B) + (a3*crossprod E3 B)
   # where E1=[1;0;0], E2=[0;1;0], E3=[0;0;1] and (*) and (+) are scalarmult and addv functions respectively. #
   # (-) is subv operator. (+) is commutative and (-) is anticommutative #
   = ( a1*(b2*E3 - b3*E2) ) + ( a2*(b3*E1 - b1*E3) ) + ( a3*(b1*E2 - b2*E1) )
   # using distributive property of (*) over operands of (+) and (-) #
   = ( a1*b2*E3 - a1*b3*E2 ) + (a2*b3*E1 - a2*b1*E3) + (a3*b1*E2 - a3*b2*E1)
   # using commutative property of unary operators 'a*' and 'b*'. and after rearrangement.#
   = (b1*a3*E2 - b1*a2*E3) + ( b2*a1*E3 - b2*a3*E1 ) + (b3*a2*E1 - b3*a1*E2)
   # using distributive property of (*) #
   = b1*( a3*E2 - a2*E3 ) + b2*( a1*E3 - a3*E1 ) + b3*( a2*E1 - a1*E2 )
   # using anti-commutatitvity of (-) i.e X-Y = opp Y-X
   = b1*opp( a2*E3 - a3*E2 ) + b2*opp( a3*E1 - a1*E3 ) + b3*opp( a1*E2 - a2*E1 )
   # using commutativity of 'opp' with (*) and distributivity of 'opp' over operands of (+) and(-).
   = opp( b1*( a2*E3 - a3*E2 ) + b2*( a3*E1 - a1*E3 ) + b3*( a1*E2 - a2*E1 ) )
   = opp (crossprod B A)
   Since A and B are arbitrary, it's true for all vectors in R^3
   
  
  Q.E.D
   
*)*)*)*)*) 


type 'a matrix = ('a list) list;; 
let rec valid_matrix n_rows (m:'a matrix) : bool = match m with(*checks if given matrix is valid matrix by checking if number of rows in each col is same*)
    [] -> true
  | first_col::rest -> if ((length first_col)=n_rows) then (valid_matrix n_rows rest) else (false)
;;

let rec determinant (m:float matrix) : float =  (* evaluates the determinant of given square matrix, raises DimError for non-square matrix*)
  let m_ = map (fun (head::tail)->tail) m in (*top row removed*)
  let rec delete_col index mat = ( match mat with (*deletes index column from given mat*)
        first_col::rest ->  if (index=0) then (rest)
          else (first_col::( delete_col (index-1) rest) )
    ) in
  let rec iterate start end_ agg l : float = ( if (start<=end_) then (
      match l with (a::r)::rest -> ( let d = determinant (delete_col start m_) in
                                     if (start mod 2=0) then (iterate (start+1) end_ ( agg +.(a*.d) ) rest
                                                             ) else( iterate (start+1) end_ ( agg -.(a*.d) ) rest
                                       
                                                                   )
                                   )
    ) else (agg)
    ) in ( match m with 
        [] -> 1.0
      | [f]::([s]::rest) -> (raise DimError) (*oversized basis set case, where eventually you reach 1xn matrix, n>1*)
      | (f::(s::rest))::[] -> (raise DimError)  (*undersized basis set case, where eventually you reach nx1 matrix,n>1*)
      | (a::r)::rest -> iterate 0 ((length m)-1) 0.0 m 
    ) ;;


          
let rotate v (basis:float matrix) : vector =  (*change of basis using cramer's formula*) (*DimError if basis are not independent*)
  let d:float = (determinant basis) in
  let rec replace index vec mat = (  (*replaces index column of mat with vec*)
    match mat with
      head::tail -> ( if (index=0) then ( vec::tail) 
                      else ( head::(replace (index-1) vec tail)
                           ) 
                    )
  ) in 
  let rec compute_coord index stop det = (if (index<=stop) then (
      (determinant (replace index v basis))/.det :: (compute_coord (index+1) stop det)
    ) else ([])
    ) in 
  if ((abs_float d)<0.0000001) then (raise DimError )(* singular matrix, thus given vectors are dependent*) 
  else (
    if (valid_matrix (length v) (v::basis)) then (
      compute_coord 0 ((length v)-1) d )
    else (raise DimError)
  );;



let boxprod v1 v2 v3 : float = dotprod v1 (crossprod v2 v3);; (* v1 v2 v3 need to be 3D vectors *)
                                                              
                                                              

(* ========================= TEST CASES =======================*)

let z0 : vector = zero 0;;  (* 0 Dimensional zero vector *)  (* output= val z0 : vector = []*)
let z3 : vector = zero 3;;  (* 3 Dimensional zero vector *)  (* output= val z3 : vector = [0.;0.;0.]*)
let z4 : vector = zero 4;;  (* 4 Dimensional zero vector *)  (* output= val z4 : vector = [0.;0;0.;0.]*)
                            
dim z0;;  (*output= - : int = 0*)
dim z3;;  (*output= - : int = 3*)
dim z4;;  (*output= - : int = 4*)
          
opp z0;;  (*output= - :vector = []*)
opp [1.0;2.0];; (*output= - : vector = [-1.; -2.]*)
                
addv [4.0;6.0] [6.0;4.0];; (*output= - : vector = [10.; 10.]*)
addv z0 z3;;  (*output= Exception : DimError.*)
addv z3 z4;;  (*output= Exception : DimError.*)
              
subv [4.0;6.0] [6.0;4.0];; (*output= - : vector = [-2.; 2.]*)
subv z0 z3;;  (*output= Exception : DimError.*)
subv z3 z4;;  (*output= Exception : DimError.*)    
              
scalarmult 2.0 [4.0; 5.0];; (*output= - : vector = [8.; 10.]*)
scalarmult 0. [4.0; 5.0];; (*output= - : vector = [0.; 0.]*)
                           
dotprod [2.; 5.; 7.] [3. ; 6. ;8.];;  (*output= - : vector = [6.; 30.; 56.]*)
dotprod [5.] [2.; 3.;];;  (*output= Exception : DimError.*)
                          
normalise [];;  (*output= - : vector = []*)
normalise [3.; 4.];;  (*output= - : vector = [0.6; 0.8]*)
                      
parallel [] [];; (*output= - : bool = true*)
parallel [1.0; 1.0] [4.0; 4.0];;  (*output= - : bool = true*)
parallel [1.0; 1.0] [4.0; -4.0];;  (*output= - : bool = false*)
parallel [4.; 5.] [1.; 9.; 4.; 7.];; (*output= Exception : DimError.*)
                                   
crossprod [1.0;0.;0.;] [0.0;1.0;0.0];; (*output= - : vector = [0.; 0.; 1.]*)
crossprod [1.; 2.; 3.] [3.; 2.; 1.];;  (*output= - : vector = [-4.; 8.; -4.]*)
crossprod [4.] [2.3; 4.5; 3.];; (*output= Exception : DimError.*)
crossprod [34.; 45.] [4.; 3.];; (*output= Exception : DimError.*) 
(*some examples that satisfy crossprod v1 v2 = opp(crossprod v2 v1)*)
crossprod [1.; 2.; 3.] [3.; 2.; 1.] = opp(crossprod [3.; 2.; 1.] [1.; 2.; 3.]) ;;  (*output= - : bool = true*)
crossprod [1.0;0.;0.;] [0.0;1.0;0.0] = opp(crossprod [0.0;1.0;0.0] [1.0;0.;0.;]) ;;  (*output= - : bool = true*)
crossprod [99.;55.5;66.7] [45.5; 88.; 33.] = opp(crossprod [45.5; 88.; 33.] [99.;55.5;66.7]) ;;  (*output= - : bool = true*)
 
rotate [1.;2.] [[0.;1.];[1.;0.]];;  (*output= - : vector = [2.; 1.]*)  
rotate [1.;0.;0.] [[1.;1.;1.];[1.;2.;1.];[0.;0.;1.]];;  (*output= - : vector = [2.; -1.; -1.]*)
rotate [2.;9.;33.;1.] [[1.;1.;1.;1.];[1.;2.;3.;4.];[2.;3.;1.;0.];[5.;2.;4.;1.]];;  (*output= - : vector = [-145.3; 32.8; 19.5; 15.1]*)
rotate [1.;2.;3.] [[1.;1.;1.;1.];[2.;2.;2.;2.];[3.;1.;4.;2.];[4.;9.;1.;0.]];;(*output= Exception: DimError.*)(*output is as expected since basis are dependent*)
rotate [1.;2.;3.] [[4.;2.;3.];[1.;0.;0.];[2.;0.;1.];[1.;1.;1.;]];;  (*output= Exception: DimError.*) (*as expected since the basis set was oversized,so dependent*)
rotate [1.;2.] [[1.;2.;3.];[1.;0.;0.];[2.;1.;3.]];;  (*output= Exception: DimError.*) (*as expected since given vector was 2D and basis were 3D*)
                                
boxprod [1.;0.;0.] [0.;1.;0.] [0.;0.;1.];;  (*output= - : float = 1.*)
boxprod [1.;2.;3.] [1.;2.;3.] [1.;1.;1.];;  (*output= - : float = 0.*)
boxprod [1.;2.;3.] [0.;2.;0.] [1.;5.;4.];;  (*output= - : float = 2.*)
boxprod [1.;2.;1.] [2.;3.;4.] [4.];; (*output= Exception : DimError.*)
boxprod [1.;2.] [1.;1.] [2.;3.];;  (*output= Exception : DimError.*) 