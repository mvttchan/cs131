let subset_test0 = subset [] [1]
let subset_test1 = not(subset [1] [])
let subset_test2 = subset [1;1;1] [1]
let subset_test3 = subset [1] [1;1;1]
let subset_test4 = subset [1] [1;1;1;2;3;4;5]
let subset_test5 = not(subset [1;1;1;2;3;4;5] [1])

let equal_sets_test0 = equal_sets [] []
let equal_sets_test1 = equal_sets [1] [1;1;1;1;1]
let equal_sets_test2 = equal_sets [1;1;1;1;1] [1]
let equal_sets_test3 = equal_sets [1;3;3] [1;3]
let equal_sets_test4 = equal_sets [1;4] [1;1;4;4]
let equal_sets_test5 = not(equal_sets [1;4] [1;2;3;4])

let set_union_test0 = equal_sets (set_union [] [1;2;2]) [1;2]
let set_union_test1 = equal_sets (set_union [1;2;2;3] []) [1;2;3]
let set_union_test2 = equal_sets (set_union [1;2;2;3] [1;2;2]) [1;2;3]

let set_intersection_test0 = equal_sets (set_intersection [1;3;5;7] [2;4;6;8]) []
let set_intersection_test1 = equal_sets (set_intersection [1;3;4] [2;3;4]) [3;4]
let set_intersection_test2 = equal_sets (set_intersection [2;5;7;7] [7]) [7]

let set_diff_test0 = equal_sets (set_diff [1;2;3;5] []) [1;2;3;5]
let set_diff_test1 = equal_sets (set_diff [] [1]) []
let set_diff_test2 = equal_sets (set_diff [1;2;2;4] [2]) [1;4]
let set_diff_test3 = equal_sets (set_diff [5;4;3;2;1] [1;2;3;4;5]) []

let computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x * 1) 10 = 10
let computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x *. 2.) 1. = infinity
let computed_fixed_point_test2 =
  computed_fixed_point (=) (fun x -> x / 2) 20 = 0

type notawksub_nonterminals =
  |Expr |Binop |Term_Binop | Term |Num

let notawksub_rules =
  [Expr, [N Num];
   Expr, [N Num; N Binop; N Num];
   Binop, [T "+"];
   Binop, [T "-"];
   Term_Binop, [T "*"];
   Term_Binop, [T "/"];
   Term, [N Num; N Term_Binop; N Num];
   Num, [T "0"]]

let notawksub_grammar = Expr, notawksub_rules

let filter_reachable_test0 =
  filter_reachable notawksub_grammar =
   (Expr,
     [Expr, [N Num];
      Expr, [N Num; N Binop; N Num];
      Binop, [T "+"];
      Binop, [T "-"];
      Num, [T "0"]])

let filter_reachable_test1 =
  filter_reachable (Term_Binop, notawksub_rules) =
    (Term_Binop,
     [Term_Binop, [T "*"];
      Term_Binop, [T "/"]])

let filter_reachable_test2 =
  filter_reachable (Expr, List.tl (List.tl notawksub_rules)) =
    (Expr, [])
