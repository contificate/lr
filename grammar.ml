
type var = string

type symbol =
  | Terminal of var
  | NonTerminal of var

let show_symbol = function
  | Terminal t | NonTerminal t -> t

module Htbl = Hashtbl
module Hset = Hashset

type t = {
    rules: (var, symbol list) Htbl.t;
    nonterms: var Hset.t
  }

let create () = {
    rules = Htbl.create 30;
    nonterms = Hashset.create 30
  }

let add_rule g x ys =
  Htbl.add g.rules x ys;
  Hset.add g.nonterms x

let symbols g =
  let syms = Hset.create 30 in
  let collect x ys =
    Hset.add syms (NonTerminal x);
    List.iter (Hset.add syms) ys
  in
  Htbl.iter collect g.rules;
  Hset.fold (fun a b -> a :: b) syms []

let productions g x =
  Htbl.fold
    (fun x' ys l ->
      if x = x' then (x, Array.of_list ys) :: l else l)
    g.rules []

let iter f g = Htbl.iter f g.rules
