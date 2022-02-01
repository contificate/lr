
module G = Grammar

let g = G.create ()

let t t = G.Terminal t
let nt t = G.NonTerminal t

let () =
  let xs = [
      ("E", [t "("; nt "O"; t ")"]);
      ("O", []);
      ("O", [nt "L"]);
      ("L", [t "int"]);
      ("L", [t "int"; t "c"; nt "L"]);
    ] in
  List.iter (fun (x, ys) -> G.add_rule g x ys) xs

let (>>) f g x = g (f x)

let () =
  let (items, transitions) =
    Lr.items g
      ("S", [|nt "E"|], 0, "$")
  in
  print_endline "digraph automaton {\nrankdir = LR;\nnode [shape=rect];";
  let print_node (i, h) =
    let lbl =
      String.concat "\\n"
        (List.map Lr.show_item (Lr.IS.elements i))
    in
    Printf.printf "%d [label=\"%s\"];\n" h lbl
  in
  let print_edge (n, s) n' =
    let s = G.show_symbol s in
    Printf.printf "%d -> %d [label=\"%s\"];\n" n n' s
  in
  Lr.ISS.iter print_node items;
  Lr.ED.iter print_edge !transitions;
  print_endline "}"
  
