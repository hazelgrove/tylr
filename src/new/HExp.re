// ex: 1 + 2 _ 3 // Bin(Op(1), Plus, Bin(Op(2), BinHole, Op(3)))
type t = HTerm.t(op, pre, post, bin)
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

type tile = Tile.t(op, pre, post, bin);
// ex: 1 + 2 _ 3 // [Op(1), Bin(Plus), Op(2), Bin(BinHole), Op(3)]
type tiles = list(tile);

type selection = HSelection.t(tile);

include HTerm.Make({
  type nonrec op = op;
  type nonrec pre = pre;
  type nonrec post = post;
  type nonrec bin = bin;

  type nonrec t = t;
  type nonrec tile = tile;

  let precedence = precedence;
  let associativity = associativity;

  let of_htiles = of_htiles;
  let of_htile = of_htile;

  let to_htiles = to_htiles;
  let to_htile = to_htile;
});

let mk_tile =
    (open_: HTessera.open_, ts: tiles, close: HTessera.close): option(tile) =>
  switch (open_, close) {
  | (Paren_l, Paren_r) =>
    let body = mk(ts);
    Some(Op(Paren(body)));
  | (Let_eq(p), Let_in) =>
    let p = HPat.mk(p);
    let def = mk(ts);
    Some(Pre(Let(p, def)));
  | _ => None
  };
