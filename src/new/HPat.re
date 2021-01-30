type t = HTerm.t(op, pre, post, bin)
and op =
  | OpHole
  | Var(Var.t)
  | Paren(t)
and pre = unit // empty
and post =
  | Ann(HTyp.t)
and bin =
  | BinHole;

exception Void_pre;

type tile = Tile.t(op, pre, post, bin);
type tiles = list(tile);

let precedence: tile => int =
  Tile.get(
    _ => 0,
    () => raise(Void_pre),
    fun
    | Ann(_) => 2,
    fun
    | BinHole => 1,
  );

let associativity =
  [(1, Associativity.Left)] |> List.to_seq |> IntMap.of_seq;

// TODO fix type signature
let rec mk = (ts: HTile.s): option(t) => {
  let rec go = (skel: Skel.t): option(t) => {
    let t = List.nth(ts, Skel.root_index(skel));
    switch (skel) {
    | Op(_) =>
      let+ op =
        switch (Tile.get_op(t)) {
        | OpHole => Some(OpHole)
        | Text(s) => StringUtil.is_var(s) ? Some(Var(s)) : None
        | Paren(body) =>
          let+ body = mk(body);
          Some(Paren(body));
        };
      Some(Op(op));
    | Pre(_, r) =>
      let+ pre =
        switch (Tile.get_pre(t)) {
        | Lam(_)
        | Let(_) => None
        }
      and+ r = go(r);
      Some(Pre(pre, r));
    | Post(l, _) =>
      let+ l = go(l)
      and+ post =
        switch (Tile.get_post(t)) {
        | Ap(_) => None
        | Ann(ann) =>
          let+ ann = HTyp.mk(ann);
          Some(Ann(ann));
        };
      Some(Post(l, post));
    | Bin(l, _, r) =>
      let+ l = go(l)
      and+ bin =
        switch (Tile.get_bin(t)) {
        | Plus
        | Arrow => None
        | BinHole => Some(BinHole)
        }
      and+ r = go(r);
      Some(Bin(l, bin, r));
    };
  };
  let* skel = associate(ts);
  go(skel);
};
