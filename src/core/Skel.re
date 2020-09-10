type t =
  | Operand(int)
  | PreOp(int, t)
  | PostOp(t, int)
  | BinOp(t, int, t);

let root_index =
  fun
  | Operand(n)
  | PreOp(n, _)
  | PostOp(_, n)
  | BinOp(_, n, _) => n;
