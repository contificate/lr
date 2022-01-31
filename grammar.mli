
type var = string

type symbol =
  | Terminal of var
  | NonTerminal of var

module Htbl = Hashtbl
module Hset = Hashset

type t = {
    rules: (var, symbol list) Htbl.t;
    nonterms: var Hset.t
  }

(** create an empty grammar *)
val create : unit -> t

(** add a production X → Y1 Y2 ... Yn *)
val add_rule : t -> var -> symbol list -> unit

(** collect list of symbols in grammar *)
val symbols : t -> symbol list

(* collect list of productions for a given non-terminal *)
val productions : t -> var -> (var * symbol array) list

(** iterate over each production *)
val iter : (var -> symbol list -> unit) -> t -> unit

