let t1 = T
let result1 = evalb t1
let code1 = compileb t1
let stk_result1 = stkmcb [] code1

let t2 = F
let result2 = evalb t2
let code2 = compileb t2
let stk_result2 = stkmcb [] code2

let t3 = Not T
let result3 = evalb t3
let code3 = compileb t3
let stk_result3 = stkmcb [] code3

let t4 = Not F
let result4 = evalb t4
let code4 = compileb t4
let stk_result4 = stkmcb [] code4

let t5 = And (T, F)
let result5 = evalb t5
let code5 = compileb t5
let stk_result5 = stkmcb [] code5
let t6 = Or (T, F)
let result6 = evalb t6
let code6 = compileb t6
let stk_result6 = stkmcb [] code6

let t7 = Implies (T, F)
let result7 = evalb t7
let code7 = compileb t7
let stk_result7 = stkmcb [] code7

let t8 = Iff (T, F)
let result8 = evalb t8
let code8 = compileb t8
let stk_result8 = stkmcb [] code8

let t9 = Not (And (T, Or (F, T)))
let result9 = evalb t9
let code9 = compileb t9
let stk_result9 = stkmcb [] code9

let t10 = Or (Implies (T, F), And (Not F, T))
let result10 = evalb t10
let code10 = compileb t10
let stk_result10 = stkmcb [] code10
