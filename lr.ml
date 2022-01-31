
module G = Grammar
type var = G.var

type item =
  var * G.symbol array * int * var

module IS =
  Set.Make(struct
      type t = item
      let compare = compare
    end)

module Htbl = Hashtbl
module Hset = Hashset
module SS = Set.Make(String)
module SM = Map.Make(String)

let nullable g =
  let null = Hset.create 30 in
  (* collect base case, X → ε *)
  G.iter
    (fun x -> function
      | [] -> Hset.add null x
      | _  -> ()) g;
  let nullable = function
    | G.NonTerminal x -> Hset.mem null x
    | _ -> false
  in
  (* find X → Y1 Y2 ... Yn, where 
     all Ys are nullable until fixpoint *)
  let changing = ref true in
  while !changing do
    let prev = Hset.cardinal null in
    let step x ys =
      if List.for_all nullable ys then
        Hashset.add null x
    in
    G.iter step g;
    changing := Hset.cardinal null > prev
  done;
  Hset.fold SS.add null SS.empty 

let compute_nullable (g : G.t) =
  let null = nullable g in
  (function
   | G.NonTerminal x ->
      SS.mem x null
   | _ -> false)

let first (g : G.t) : SS.t SM.t =
  let map = ref SM.empty in
  let nullable = compute_nullable g in
  let first = function
    | G.Terminal y -> SS.singleton y
    | G.NonTerminal x ->
       SM.find x !map
  in
  (* initialise FIRST(X) = ∅ *)
  G.iter
    (fun x _ ->
      map := SM.add x SS.empty !map) g;
  (* cardinality summation *)
  let size () =
    SM.fold (fun _ s -> (+) (SS.cardinal s)) !map 0
  in
  let changing = ref true in
  while !changing do
    let prev = size () in
    let step x =
      let rec go = function
        | y :: ys when nullable y -> go ys
        | t :: _ ->
           map :=
             SM.add x
               (SS.union (first t) (SM.find x !map)) !map
        | _ -> ()
      in go
    in
    G.iter step g;
    changing := size () > prev
  done;
  !map

let compute_first g =
  let first = first g in
  (function
   | G.NonTerminal t -> SM.find t first
   | G.Terminal t -> SS.singleton t)

let closure g =
  let go first nullable i =
    let set = ref i in
    let changing = ref true in
    let size () = IS.cardinal !set in
    while !changing do
      let prev = size () in
      let close (_, ys, i, l) =
        if i >= Array.length ys then ()
        else
          (match ys.(i) with
           | G.NonTerminal b ->
              let prods = G.productions g b in
              let rest =
                let i' = i + 1 in
                Array.(to_list (sub ys i' (length ys - i')))
              in
              let rec follow = function
                | t :: ts when nullable t -> follow ts
                | t :: _ -> first t
                | [] -> SS.empty
              in
              (* [A → α.Bβ, l] *)
              let beta = follow rest in
              let lookaheads =
                (* if all of β is nullable, then lookahead is immediate *)
                SS.(if is_empty beta then [l] else elements beta)
              in
              (* add fresh initial items for all productions [A → a.Bβ, l] *)
              List.
              (iter
                 (fun (x, ys) ->
                   iter (fun l -> set := IS.add (x, ys, 0, l) !set) lookaheads) prods)
           | _ -> ())
      in
      IS.iter close !set;
      changing := size () > prev 
    done;
    !set
  in
  go (compute_first g) (compute_nullable g)

(*
 * TODO: Implement items, goto, etc.
 * Ensure no recomputation of first or nullable.
 *)
