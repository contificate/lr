open Js_of_ocaml

module G = Grammar

let (>>) f g x = g (f x)

let graphviz (items, transitions, names) =
  let open Printf in
  let buf = Buffer.create 1000 in
  bprintf buf "digraph automaton {\nrankdir = LR;\nnode [shape=rect];bgcolor=\"#f9f9f9\";\n";
  let print_node (i, h) =
    let lbl =
      String.concat "\\n"
        (List.map Lr.show_item (Lr.IS.elements i))
    in
    bprintf buf "%d [label=\"%d:\\n%s\"];\n" h h lbl
  in
  let id i = Lr.IM.find i names in
  let print_edge (n, s) n' =
    let s = G.show_symbol s in
    let n, n' = id n, id n' in 
    bprintf buf
      "%d -> %d [label=\"%s\"];\n" n n' s
  in
  Lr.ISS.iter print_node items;
  Lr.ED.iter print_edge transitions;
  bprintf buf "}";
  Buffer.to_bytes buf
  |> String.of_bytes

let rec json_of_action : Lr.action -> Yojson.t =
  function
  | Shift s ->
     `Assoc [("s", `Int s)]
  | Reduce (x, ys) ->
     let ys = Array.to_list ys in
     `Assoc [("r", `List (`String x :: (List.map (fun y -> `String (G.show_symbol y) ) ys)))]
  | Conflict (l, r) ->
     let l, r = json_of_action l, json_of_action r in
     `Assoc [("c", `List [l;r])]
  | Accept -> `String "a"

let json_of_action_tbl : (int * string, Lr.action) Hashtbl.t -> Yojson.t = fun action ->
  let entry (st, sym) act =
    `List [`Int st; `String sym; json_of_action act]
  in
  let entries =
    Hashtbl.fold
      (fun k v l -> entry k v :: l) action []
  in
  `List entries

let json_of_goto_tbl : (int * string, int) Hashtbl.t -> Yojson.t = fun goto ->
  let entry (i, x) j =
    `List [`Int i; `String x; `Int j]
  in
  let entries =
    Hashtbl.fold
      (fun k v l -> entry k v :: l) goto []
  in
  `List entries

let parse_bnf =
  Lexing.from_string
  >> Parser.rules Lexer.tokenise

let construct grammar : Yojson.t =
  let rules = parse_bnf grammar in
  let g = G.create () in
  List.iter
    (fun (x, ys) -> G.add_rule g x ys) rules;
  let tbl =
    Lr.table g
      ("S'", [|G.NonTerminal "S"|], 0, "$")
  in
  let (terminals, nonterminals) : Yojson.t * Yojson.t =
    (fun (l, r) -> (`List l, `List r))
    @@ List.partition_map (function
        | G.Terminal t -> Either.Left (`String t)
        | G.NonTerminal t -> Either.Right (`String t)
      ) (G.symbols g)
  in
  let automaton =
    `String (graphviz (tbl.items, tbl.edges, tbl.names))
  in
  let action = json_of_action_tbl tbl.action in
  let goto = json_of_goto_tbl tbl.goto in
  `Assoc [
      ("count", `Int (Lr.ISS.cardinal tbl.items));
      ("terminals", terminals);
      ("nonterminals", nonterminals);
      ("action", action);
      ("goto", goto);
      ("automaton", automaton)
    ]
  
let () =
  Js.export "lr"
    (object%js
       method construct =
         Js.to_string
         >> construct
         >> Yojson.to_string
         >> Js.string
     end)
