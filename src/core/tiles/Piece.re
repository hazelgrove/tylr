[@deriving sexp]
type t =
  | Grout(Grout.t)
  | Shard(Shard.t);
