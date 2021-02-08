type s = list(t)
and t = Tile.t(op, pre, post, bin)
and op =
  | OpHole
  | Text(string)
  | Paren(s)
and pre =
  | Lam(s)
  | Let(s, s)
and post =
  | Ap(s)
  | Ann(s)
and bin =
  | BinHole
  | Plus
  | Arrow;
