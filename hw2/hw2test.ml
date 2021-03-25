let accept_all string = Some string

let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

type giant_nonterminals =
  | Conversation | Sentence | Grunt | Snore | Shout | Quiet

let giant_grammar =
  Conversation,
  [Snore, [T"ZZZ"];
   Quiet, [];
   Grunt, [T"khrgh"];
   Shout, [T"aooogah!"];
   Sentence, [N Quiet];
   Sentence, [N Grunt];
   Sentence, [N Shout];
   Conversation, [N Snore];
   Conversation, [N Sentence; T","; N Conversation]]

let hw2_grammar = convert_grammar giant_grammar;;
let frag = ["khrgh"; ","; "khrgh"; ","; "ZZZ"]

let make_matcher_test =
  ((make_matcher hw2_grammar accept_all frag) = Some [])

let make_parser_test =
  ((make_parser hw2_grammar frag) =
    Some (Node (Conversation,
     [Node (Sentence, [Node (Grunt, [Leaf "khrgh"])]); Leaf ",";
      Node (Conversation,
       [Node (Sentence, [Node (Grunt, [Leaf "khrgh"])]); Leaf ",";
        Node (Conversation, [Node (Snore, [Leaf "ZZZ"])])])])))
