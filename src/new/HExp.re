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

let flatten_tile: tile => AltList.t(HTessera.t, t) =
  Tile.get(
    fun
    | OpHole => A(OpHole, None)
    | Num(n) => A(Num(n), None)
    | Var(x) => A(Var(x), None)
    | Paren(body) => A(Paren_l, Some(B(body, A(Paren_r, None)))),
    fun
    | Lam(p) => A(Lam(HPat.to_utiles(HPat.flatten(p))), None)
    | Let(p, def) =>
      A(
        Let_eq(HPat.to_utiles(HPat.flatten(p))),
        Some(B(def, A(Let_in, None))),
      ),
    fun
    | Ap(_) => failwith("todo"),
    fun
    | Plus => A(Plus, None)
    | Arrow => A(Arrow, None),
  );

let precedence: tile => int =
  Tile.get(
    _ => 0,
    fun
    | Lam(_) => 10
    | Let(_) => 11,
    fun
    | Ap(_) => 1,
    fun
    | Plus(_) => 3
    | BinHole => 2,
  );

let associativity =
  [(2, Associativity.Left), (3, Left)] |> List.to_seq |> IntMap.of_seq;

let rec of_htiles = (ts: HTile.s): option(tiles) =>
  ts |> List.map(of_htile) |> OptUtil.sequence
and of_htile = (t: HTile.t): option(I.tile) =>
  t
  |> Tile.get(
       fun
       | OpHole => Some(Tile.Op(OpHole))
       | Text(s) =>
         if (StringUtil.is_num(s)) {
           Some(Op(Num(int_of_string(s))));
         } else if (StringUtil.is_var(s)) {
           Some(Op(Var(s)));
         } else {
           None;
         }
       | Paren(body) => {
           let+ body = of_htiles(body);
           Tile.Op(Paren(body));
         },
       fun
       | Lam(p) => {
           let+ p = Pat.of_htiles(p);
           Tile.Pre(Lam(p));
         }
       | Let(p, def) => {
           let+ p = Pat.of_htiles(p)
           and+ def = of_htiles(def);
           Tile.Pre(Let(p, def));
         },
       fun
       | Ann(_) => None
       | Ap(arg) => {
           let+ arg = of_htiles(arg);
           Tile.Post(Ap(arg));
         },
       fun
       | Arrow => None
       | Plus => Some(Tile.Bin(Plus))
       | BinHole => Some(Bin(BinHole)),
     );

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
