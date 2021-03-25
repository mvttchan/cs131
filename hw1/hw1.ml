type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let rec subset a b = match a with
    | [] -> true
    | head::tail -> if List.mem head b then subset tail b
                    else false

let equal_sets a b = (subset a b) && (subset b a)

let rec set_union a b = match a with
    | [] -> b
    | head::tail -> if List.mem head b then set_union tail b
                    else set_union tail ([head]@b)

let rec set_intersection a b = List.filter (fun x -> (List.mem x b)) a

let rec set_diff a b = List.filter (fun x -> not (List.mem x b)) a

let rec computed_fixed_point eq f x = if eq (f x) x then x
                                      else computed_fixed_point eq f (f x)

let rec get_nonterminals rules = match rules with
    | [] -> []
    | T head::tail -> get_nonterminals tail
    | N head::tail -> head::(get_nonterminals tail)

let rec add_current_reachable expression_list rules = match rules with
    | [] -> expression_list
    | head::tail -> if List.mem (fst head) expression_list then
                      let temp_set = get_nonterminals(snd head) in
                      let new_set = set_union temp_set expression_list in
                      add_current_reachable new_set tail
                    else
                      add_current_reachable expression_list tail

let make_list expression_list rules =
    computed_fixed_point equal_sets (fun list -> add_current_reachable list rules) expression_list

let filter_unreachable start_expression rules =
  let reachable = make_list [start_expression] rules in
  List.filter (fun r -> (List.mem (fst r) reachable)) rules

let filter_reachable g =
  let start_expression = fst g in
  let rules      = snd g in
  (start_expression, filter_unreachable start_expression rules)
