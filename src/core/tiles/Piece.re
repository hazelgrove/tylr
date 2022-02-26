[@deriving sexp]
type t =
  | Grout(Grout.t)
  | Shard(Shard.t);

let sort =
  fun
  | Grout(g) => g.sort
  | Shard(_) => failwith("todo sort");

let nibs =
  fun
  | Grout(g) => g.nibs
  | Shard(s) => s.nibs;
