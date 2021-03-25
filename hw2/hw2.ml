type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let rec conversion old_rules nonterminal = match old_rules with
    | [] -> []
    | head::tail -> if (fst head) = nonterminal
                    then (snd head)::(conversion tail nonterminal)
                    else conversion tail nonterminal

let convert_grammar gram1 = ((fst gram1), conversion (snd gram1))

let rec parse_tree_helper tree = match tree with
    | [] -> []
    | head::tail -> match head with
                      | Leaf leaf -> leaf::(parse_tree_helper tail)
                      | Node (_, subtree) ->
                         (parse_tree_helper subtree) @ (parse_tree_helper tail)

let parse_tree_leaves tree = parse_tree_helper [tree]

let rec make_matcher_helper production_function rules accept frag = match rules with
    | [] -> None
    | rules_head::rules_tail ->
       let interim = make_match production_function rules_head accept frag in
       match interim with
       | None -> make_matcher_helper production_function rules_tail accept frag
       | _    -> interim
and make_match production_function rule accept frag = match rule with
  | (T prefix)::suffix -> if (frag = []) then None
                          else if (List.hd(frag) = prefix) then make_match production_function suffix accept (List.tl(frag))
                          else None
  | (N prefix)::suffix ->
     let new_rules = production_function prefix in
     let new_accept = make_match production_function suffix accept in
     make_matcher_helper production_function new_rules new_accept frag
  | [] -> accept frag

let make_matcher gram =
  let start = fst gram in
  let production_function = snd gram in
  let rules = production_function start in
  fun accept frag -> make_matcher_helper production_function rules accept frag

let rec top_level nonterminal p_function rules accept frag acc = match rules with
   | [] -> None
   | head::tail ->
      let derivation = derive p_function head accept frag ((nonterminal, head)::acc) in
      match derivation with
        | None -> top_level nonterminal p_function tail accept frag acc
        | anything -> anything
and derive p_function rule accept frag acc = match rule with
    | [] -> accept frag acc
    | r_head::r_tail -> match frag with
             | [] -> None
             | f_head::f_tail -> match r_head with
                      | N sym ->
                         let new_rules = p_function sym in
                         let new_accept = derive p_function r_tail accept in
                         top_level sym p_function new_rules new_accept frag acc
                      | T ter -> if f_head = ter then derive p_function r_tail accept f_tail acc
                                         else None

let rec parse_big rule = match rule with
    | [] ->
       let a = fst (List.hd rule) in
       let x = parse_small rule [] in
       let b = fst x in
       let c = snd x in
       Node (a, b), c
    | head::tail ->
       let a = fst head in
       let x = parse_small tail (snd head) in
       let b = fst x in
       let c = snd x in
       Node (a,b), c
and parse_small rule acc = match acc with
  | [] -> [], rule
  | head::tail -> match head with
                    | N _ ->
                       let a = fst (parse_big rule) in
                       let x = parse_small (snd (parse_big rule)) tail in
                       let b = fst x in
                       let c = snd x in
                       a::b, c
                    | T a ->
                       let x = parse_small rule tail in
                       let b = fst x in
                       let c = snd x in
                       (Leaf a)::b, c
let make_parser gram frag =
  let internal_acceptor fragment derivation = match fragment with
    | _::_ -> None
    | elem -> Some (elem, derivation) in
  let start = fst gram in
  let production_function = snd gram in
  let rules = production_function start in
  let path = top_level start production_function rules internal_acceptor frag [] in
  match path with
    | Some (_, rule) -> Some (fst (parse_big (List.rev rule)))
    | _ -> None
