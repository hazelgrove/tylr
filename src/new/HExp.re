module Tile = {
  type op =
    | OpHole
    | Num(int)
    | Var(Var.t)
    | Paren(s);
  type pre =
    | Lam(s)
    | Let(s, s);
  type post =
    | Ap(s);
  type bin =
    | BinHole
    | Plus;
  type t = Tile.t(op, pre, post, bin);
};

type t =
  | TypeIncon(t)
  | Op(op)
  | Pre(pre, t)
  | Post(t, post)
  | Bin(t, bin, t)
and op =
  | OpHole
  | Num(int)
  | Var(Var.t)
  | Paren(t)
and pre =
  | Lam(HPat.t)
  | Let(HPat.t, t)
and post =
  | Ap(t)
and bin =
  | Plus
  | BinHole;