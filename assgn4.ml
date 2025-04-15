module type SET = 

sig 

  type ( 'a ) set

  val emptyset : 'a set

  val member : 'a  -> 'a set -> bool
  
  val complement : 'a set -> 'a set -> 'a set

  val subset : 'a set -> 'a set -> bool

  val equalsets: 'a set -> 'a set -> bool

  val union :  'a set -> 'a set -> 'a set

  val inter : 'a set -> 'a set -> 'a set 

  val diff : 'a set -> 'a set -> 'a set 

  val product : ('a set) -> ('b set) -> ('a * 'b) set

  val power : 'a set -> ('a set) set

end;;

module SET : SET = struct

  open List
  type 'a set = 'a list

  let emptyset:'a set = []
  (*
  1. This function outputs an empty list which has no duplicates.
  3. Total time complexity is O(1).
  *)

  let member e s : bool = fold_left (||) false ( map (fun x -> x=e) s )
  (*
  1. This function do not output a set.
  3. Since the function checks the given element with every element while linearly progressing through set,
    so time complexity T = O(n) , where n=size of set 's'
    *)

  let complement s u(*universal set*) : 'a set = filter (fun x->not (member x s) ) u
  (*
  1. Let s and u have no duplicates. Since output is subset of u(since it's filtered from it), output also has no duplicates.
  3. - Checking member of an element in s takes O(m) time where m=size of s.
    - Doing above for all elements of u which are O(n) in number.
    - Total time complexity is O(m*n)
    *)

  let subset s1 s2 : bool = fold_left (&&) true ( map (fun x ->  (member x s2) ) s1 )
  (*
  1. This function do not output a set
  3. - checking if an element of s1 is also member of s2 takes O(n) time, where n=size of s2
    - above is repeated for all 'm' elements of s1.
    - Thus, total time complexity is O(m*n)
  *)

  let equalsets s1 s2 : bool = (subset s1 s2) && (subset s2 s1)

  (*
  1. This function do not output a set.
  3. - checking if s1 is subset of s2 takes O(m*n) time where (m=size of s1) , (n=size of s2)
    - checking if s2 is subset of s1 takes O(m*n) time
    - Total time complexity is O(m*n)
    *)


  let union s1 s2 : 'a set = s1 @ ( filter (fun x -> not (member x s1) ) s2 )

  (*
  1. Let s1 and s2 maintain representational invariance before 'union'. Then function  'union' filters elements, common to both s1 and s2, from s2 and thus remaining
    elements of s2 are not in s1. Then, it includes those filtered elements of s2 into s1 and thus maintains representational invariance of output.
  3. - filtering s2 by checking for each element of s2 if it's in s1, takes O(m*n), where (m=size of s1) , (n=size of s2)
    - appending filtered s2 to s1 takes O(m+n) time.
    - total time complexity is O(m*n)
    *)

  let inter s1 s2 : 'a set = filter (fun x -> member x s2) s1

  (*
  1. Let s1 and s2 maintain representational invariance before 'inter'. 'inter' is filtering one of the given sets. Since both sets had no duplicates initially,
    the output will have no duplicates because it is a subset of the set that is filtered.
  3. - filtering one set by checking for its elements if they're in other set, takes O(m*n), where m and n are sizes of the given sets
    - Total time complexity is O(m*n)
    *)

  let diff s1 s2 : 'a set = filter (fun x -> not (member x s2)) s1
  (*
  1. Let s1 and s2 maintain representational invariance before 'diff'. 'diff' is filtering one of the given sets. Since both sets had no duplicates initially,
    the output will have no duplicates because it is a subset of the set that is filtered.
  3. - filtering one set by checking for its elements if they're in other set, takes O(m*n), where (m=size of s1) , (n=size of s2)
    - Total time complexity is O(m*n)
  *)

  let product (s1:'a set) (s2:'b set) : ('a*'b) set =
    fold_left (fun x y -> (fold_left (fun a b -> b::a) x y)) emptyset (
      map
        (
          fun x->
            ( map (fun y->(x,y)) s2
            )
        ) s1
    ) 
  (*
  1. Let s1 and s2 have no duplicates. Then, for all x in s1, set of (x,y) for all y in s2, has no duplicates as each unique element y is mapped to unique pair (x,y).
    And two pairs (x1,y1) and (x2,y2) ,where x1,x2 are two different elements of s1, are different irrespective of values of y1 and y2. So, output of (product s1 s2)
    thus has no dupicates.
  3. - An element 'x' of s1 is mapped to {(x,y): for all y in s2}, which takes O(n) time where n=size of s2.
    - Above is done for all elements of s1.There are O(m) number of elements in s1.
    - set ,containing m sets each of size n , is flatten taking O(m*n) time.
    - So Total time complexity = O(m*n)
    *)

  let rec power s : ('a set) set = match s with
      [] -> [emptyset]
    | head::tail -> let n = (power tail) in
        (map (fun x->head::x) n)@n

  (*
  1. Claim : if 's' has no duplicates, then output of (power s) has no duplicates.
    Proof: by induction on length s
      Base case: length s = 0 , then s = emptyset, which has no duplicates.
                  For it, output is set that contains emptyset only. Thus, output has no duplicates.
      Inductive case: Let for all s:set with (length s = k), if s has no duplicates, then (power s) has no duplicates.
      Consider for all s:set with (length s = k+1), let s has no duplicates.
        Then, for some x in s, s = s' U {x}
          (power s) includes all elements of (power s') and s1.
          Here, by induction assumption since length s'=k,(power s') has no duplicates.
          And, s1 is the set of elements that are formed by including x into each element of (power s') which are all already different from each other(by induction
          assumption), and so resulting set s1 has no duplicates.
          Elements of s1 also has no duplicates because x is different from elements of s' and so from elements of subsets of s' which comprise (power s').
      Thus, proved that if 's' has no duplicates, then (power s) has no duplicates.
  2.
  3. - let T(n) be the time complexity of 'power s' where n=size of s.
    - T(n) = T(n-1) + O(2^n), where O(2^n) is due to running 'map' on 2^(n-1) sized set and then appending two such sized sets.
    - Thus, T(n) = O(2^n)
    *)


end;;


open List;;

type 'a set = 'a list;;

let emptyset:'a set = [];;
(*
1. This function outputs an empty list which has no duplicates.
3. Total time complexity is O(1).
*)

let member (e:'b) (s:'a set) : bool = fold_left (||) false ( map (fun x -> x=e) s );;
(*
1. This function do not output a set.
3. Since the function checks the given element with every element while linearly progressing through set,
  so time complexity T = O(n) , where n=size of set 's'
  *)

let complement (s:'a set) (u:'a set(*universal set*)) : 'a set = filter (fun x->not (member x s) ) u;;
(*
1. Let s and u have no duplicates. Since output is subset of u(since it's filtered from it), output also has no duplicates.
3. - Checking member of an element in s takes O(m) time where m=size of s.
  - Doing above for all elements of u which are O(n) in number.
  - Total time complexity is O(m*n)
  *)

let subset (s1:'a set) (s2:'a set) : bool = fold_left (&&) true ( map (fun x ->  (member x s2) ) s1 )  ;;
(*
1. This function do not output a set
3. - checking if an element of s1 is also member of s2 takes O(n) time, where n=size of s2
  - above is repeated for all 'm' elements of s1.
  - Thus, total time complexity is O(m*n)
*)

let equalsets (s1:'a set) (s2:'a set) : bool = (subset s1 s2) && (subset s2 s1);;

(*
1. This function do not output a set.
3. - checking if s1 is subset of s2 takes O(m*n) time where (m=size of s1) , (n=size of s2)
  - checking if s2 is subset of s1 takes O(m*n) time
  - Total time complexity is O(m*n)
  *)


let union (s1:'a set) (s2:'a set) : 'a set = s1 @ ( filter (fun x -> not (member x s1) ) s2 );;

(*
1. Let s1 and s2 maintain representational invariance before 'union'. Then function  'union' filters elements, common to both s1 and s2, from s2 and thus remaining
  elements of s2 are not in s1. Then, it includes those filtered elements of s2 into s1 and thus maintains representational invariance of output.
3. - filtering s2 by checking for each element of s2 if it's in s1, takes O(m*n), where (m=size of s1) , (n=size of s2)
  - appending filtered s2 to s1 takes O(m+n) time.
  - total time complexity is O(m*n)
  *)

let inter (s1:'a set) (s2:'a set) : 'a set = filter (fun x -> member x s2) s1 ;;

(*
1. Let s1 and s2 maintain representational invariance before 'inter'. 'inter' is filtering one of the given sets. Since both sets had no duplicates initially,
  the output will have no duplicates because it is a subset of the set that is filtered.
3. - filtering one set by checking for its elements if they're in other set, takes O(m*n), where m and n are sizes of the given sets
  - Total time complexity is O(m*n)
  *)

let diff (s1:'a set) (s2:'a set) : 'a set = filter (fun x -> not (member x s2)) s1 ;;
(*
1. Let s1 and s2 maintain representational invariance before 'diff'. 'diff' is filtering one of the given sets. Since both sets had no duplicates initially,
  the output will have no duplicates because it is a subset of the set that is filtered.
3. - filtering one set by checking for its elements if they're in other set, takes O(m*n), where (m=size of s1) , (n=size of s2)
  - Total time complexity is O(m*n)
*)


let product (s1:'a set) (s2:'b set) : ('a*'b) set =
  fold_left (fun x y -> (fold_left (fun a b -> b::a) x y)) emptyset (
    map
      (
        fun x->
          ( map (fun y->(x,y)) s2
          )
      ) s1
  )
;;
(*
1. Let s1 and s2 have no duplicates. Then, for all x in s1, set of (x,y) for all y in s2, has no duplicates as each unique element y is mapped to unique pair (x,y).
  And two pairs (x1,y1) and (x2,y2) ,where x1,x2 are two different elements of s1, are different irrespective of values of y1 and y2. So, output of (product s1 s2)
  thus has no dupicates.
3. - An element 'x' of s1 is mapped to {(x,y): for all y in s2}, which takes O(n) time where n=size of s2.
   - Above is done for all elements of s1.There are O(m) number of elements in s1.
   - set ,containing m sets each of size n , is flatten taking O(m*n) time.
   - So Total time complexity = O(m*n)
  *)

let rec power (s:'a set) : ('a set) set = match s with
    [] -> [emptyset]
  | head::tail -> let n = (power tail) in
      (map (fun x->head::x) n)@n
;;

(*
1. Claim : if 's' has no duplicates, then output of (power s) has no duplicates.
  Proof: by induction on length s
    Base case: length s = 0 , then s = emptyset, which has no duplicates.
                For it, output is set that contains emptyset only. Thus, output has no duplicates.
    Inductive case: Let for all s:set with (length s = k), if s has no duplicates, then (power s) has no duplicates.
    Consider for all s:set with (length s = k+1), let s has no duplicates.
      Then, for some x in s, s = s' U {x}
        (power s) includes all elements of (power s') and s1.
        Here, by induction assumption since length s'=k,(power s') has no duplicates.
        And, s1 is the set of elements that are formed by including x into each element of (power s') which are all already different from each other(by induction
        assumption), and so resulting set s1 has no duplicates.
        Elements of s1 also has no duplicates because x is different from elements of s' and so from elements of subsets of s' which comprise (power s').
    Thus, proved that if 's' has no duplicates, then (power s) has no duplicates.
2.
3. - let T(n) be the time complexity of 'power s' where n=size of s.
  - T(n) = T(n-1) + O(2^n), where O(2^n) is due to running 'map' on 2^(n-1) sized set and then appending two such sized sets.
  - Thus, T(n) = O(2^n)
  *)


   
   
(*=====================TEST CASES =================*)

let universe: int set = [0;1;2;3;4;5;6;7;8;9] ;;    (*universal set*)                                                                              


(*emptyset: no element belongs to emptyset*) 
member 1 emptyset;; (*output= - : bool = false*)
member "hi" emptyset;;  (*output= - : bool = false*)
member emptyset emptyset;;  (*output= - : bool = false*)
  
(*membership tests*)
member 1 [1;2;3];; (*output= - : bool = true*)
member 4 [1;2;3];; (*output= - : bool = false*) 
                   
(*Equality*)
equalsets [1;2] [1;2];; (*output= - : bool = true*)  (*equality is reflexive*)
                                                     
(*Complement tests*)
complement [1;2] universe;; (*output= - : int set = [3; 4; 5]*) 
complement emptyset universe;; (*output= - : int set = [1; 2; 3; 4; 5]*)    (*Complement of empty set is universal set*) 
complement [1;2;3;4;5] universe = emptyset;;   (*output= - : bool = true*)  (*Complement of universal set is emptyset*)
equalsets [1;2;4] (complement (complement [1;2;4] universe) universe );; (*output= - : bool = true*) (*Involution of complement*)
                                                                               
(*Subset tests*)
subset [1;2] [1;2;3;4;5];; (*output= - : bool = true*) subset [1;2;3;4;5] [1;2];;  (*output= - : bool = false*)  (*subset relationship is not symmetric*)
subset [1;2] [2;3;4;5];; (*output= - : bool = false*)
subset emptyset [1;2];; (*output= - : bool = true*)   (*Empty set is subset of all sets*)
subset emptyset emptyset;; (*output= - : bool = true*)  (*Every set is subset of itself, reflexivity of subset*)
subset [1;2] [1;2];; (*output= - : bool = true*)  (*Every set is subset of itself*)
                                                                               
(*Union*)
union [1;2] [3;2;4;5];;  (*output= - : int set = [1; 2; 3; 4; 5]*)
equalsets (union [3;4;5] [1;2]) (union [1;2] [3;4;5]);;  (*output= - : bool = true*)  (*Union is commutative*)
union emptyset [1;2;3];;  (*output= - : int set = [1; 2; 3]*)    (*Union with emptyset has no effect*)  (*Identity*)
union [1;2] [1;2;3;4;5];; (*output= - : int set = [1; 2; 3; 4; 5]*)   (* A U B = B if A is subset of B*) (*subset-union*)
equalsets (union (complement [1;4;5] [0;1;2;3;4;5]) (complement [0;5] [0;1;2;3;4;5]) ) 
  (complement (union [1;4;5] [0;5]) [0;1;2;3;4;5]);;  (*output= - : bool = true*)   (* A' U B' = (A U B)' *)
(*subset of unions*)
subset [1;2;3] (union [1;2;3] [2;3;6]);; (*output= - : bool =true*)
subset [2;3;6] (union [1;2;3] [2;3;6]);; (*output= - : bool =true*)
(*Intersection subsets*)
subset (inter [1;2] [2;3;4]) [1;2];;  (*output= - : bool = true*)
subset (inter [1;2] [2;3;4]) [2;3;4];;  (*output= - : bool = true*)  
(*Union with complement*)
equalsets (union [1;2;3] (complement [1;2;3] universe)) universe;; (*output= - : bool = true*)
                                                                                    
(*Intersection*)
inter [1;2] [1;3;4;5];; (*output= - : int set = [1]*)
equalsets (inter [3;4;5] [1;2]) (inter [1;2] [3;4;5]);;  (*output= - : bool = true*)  (*Intersection is commutative*)
equalsets emptyset (inter emptyset [1;2;6]);; (*output= - : bool = true*)  (*Intersection with emptyset is always emptyset*) (*Annihilation*)
inter [1;2] [1;2;3;4;5];;  (*output= - : int set = [1;2]*)       (* A intersection B = A if A is subset of B*)
equalsets emptyset (inter [1;2] [3;4;5]);; (*output= - : bool = true*)  (*Intersection of two disjoint sets is emptyset*)
equalsets (inter (complement [1;4;5] [0;1;2;3;4;5]) (complement [0;5] [0;1;2;3;4;5]) ) 
  (complement (inter [1;4;5] [0;5]) [0;1;2;3;4;5]);;  (*output= - : bool = true*)  (* A' intersection B' = (A intersection B)' *) (*subset-intersection*)
                                                                                                                                  
                                                                                                                                  
(*Distributivity of union over intersection*)
equalsets (union [1;2;4;9] (inter [1;3;6] [7;5;4]))
  (inter (union [1;2;4;9] [1;3;6]) (union [1;2;4;9] [7;5;4]));; (*output= - : bool = true*)
(*Distributivity of intersection over union*)
equalsets (inter [1;2;4;9] (union [1;3;6] [7;5;4]))
  (union (inter [1;2;4;9] [1;3;6]) (inter [1;2;4;9] [7;5;4]));; (*output= - : bool = true*) 
                                                                
(*Difference tests*)
diff [1;2;4] [1;2];; (*output= int set = [4]*)
diff [1;2;4] emptyset;; (*output= int set = [1;2;4]*) (*A-emptyset=A , Identity*)
(*Difference-Complement-Union*)      
equalsets (diff [1;2;3;4] [2;3]) (inter [1;2;3;4] (complement [2;3] universe));;  (*output= - : bool = true*)
                                                                                   
let universe: int set = [0;1;2;3;4;5;6] ;;    (*universal set*)                                                                              
(*De Morgan's Laws*)
equalsets (union (complement [1;2;3] universe) (complement [0;4] universe) ) 
  ( complement (inter [1;2;3] [0;4]) universe );;  (*output= - : bool = true*)    (*A' U B'  = (A intersection B)'*)
equalsets (inter (complement [1;2;3] universe) (complement [0;4] universe) ) 
  ( complement (union [1;2;3] [0;4]) universe );;  (*output= - : bool = true*)    (*A' intersection B'  = (A U B)'*)
                                                                                  
(*Cartesion product*)
product [1;2;3] [1;2];;  (*output= - : (int * int) set = [(3, 2); (3, 1); (2, 2); (2, 1); (1, 2); (1, 1)]*)
equalsets emptyset (product emptyset [1;2;3]);; (*output= - : bool= true*)   (*Cartesian product with empty set is empty*)
                                                                             
(*Power set*)
power [1;2];; (*output= - : int set set = [[1; 2]; [1]; [2]; []]*)
power emptyset;; (*output = - : int set = [[]]*)
subset (power [1;2;3]) (power universe);;  (*output= - : bool = true*)   (*if A is subset of B, then power(A) is subset of power(B)*)
                                                


(*==============GIVEN TESTCASES=========*)
member 3 [5;3;6];;
member 4 [5;3;6];;
member 4 [];;
subset [10;20] [10;30;20];;
subset [10;20;40] [10;30;20];;
subset [10;20] [10];;
equalsets [10;20] [10;20;30];;
equalsets [10;20;30] [10;30;20];;
union [10;50] [10;20;40];;
inter  [10;50] [10;20;40];;
diff  [10;50] [10;20;40];;
power [];;
power [10;30;20];;
product [1;2;3] [10;20;30];;
product [1;2;3] [];;
