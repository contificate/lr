
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
  let (items, transitions, names) =
    Lr.items g
      ("S", [|nt "E"|], 0, "$")
  in
  print_endline "digraph automaton {\nrankdir = LR;\nnode [shape=rect];";
  let print_node (i, h) =
    let lbl =
      String.concat "\\n"
        (List.map Lr.show_item (Lr.IS.elements i))
    in
    Printf.printf "%d [label=\"%d:\\n%s\"];\n" h (Lr.IM.find h names) lbl
  in
  let print_edge (n, s) n' =
    let s = G.show_symbol s in
    Printf.printf "%d -> %d [label=\"%s\"];\n" n n' s
  in
  Lr.ISS.iter print_node items;
  Lr.ED.iter print_edge transitions;
  print_endline "}"

let () =
  let tbl =
    Lr.table g
      ("S", [|nt "E"|], 0, "$")
  in
  Hashtbl.iter (fun (i, x) a -> Printf.printf "action[%d, %s] = %s\n" i x (Lr.show_action a)) tbl.action;
  Hashtbl.iter (fun (i, x) a -> Printf.printf "goto[%d, %s] = %d\n" i x a) tbl.goto;

  
  
