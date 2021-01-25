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

type t = Tile.t(op, pre, post, bin);

exception Void_pre;

module Tile = {
  type t = Tile.t(op, pre, post, bin)
  and op =
    | OpHole
    | Var(Var.t)
    | Paren(HTile.s)
  and pre = unit // empty
  and post =
    | Ann(HTile.s)
  and bin =
    | BinHole;
};

let precedence: Tile.t => int =
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

let associate = (ts: HTile.s): option(ZList.t(HTile.t, HTile.t)) => {
  if (ts == []) {
    failwith("expected ts to be nonempty");
  };
  let its = List.mapi((i, t) => (i, t), ts);
  let+ (max_p, max_p_ts) =
    List.fold_right(
      ((_, t) as it, max) => {
        let* (max_p, max_p_ts) = max;
        let* p = precedence(t);
        if (p > max_p) {
          Some((p, [it]));
        } else if (p == max_p) {
          Some((max_prec, [it, ...max_p_ts]));
        } else {
          max;
        };
      },
      its,
      Some((0, [])),
    );
  let a = IntMap.find(max_p, associativity);
  let n =
    if (List.length(max_p_ts) > 1 && a == Direction.Right) {
      // guaranteed to succeed if ts is nonempty
      let (_, (n, _)) = ListUtil.split_last(max_p_ts);
      n;
    } else {
      // guaranteed to succeed if ts is nonempty
      let (n, _) = List.hd(max_p_ts);
      n;
    };
  ZList.split_at(n, ts);
};

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
