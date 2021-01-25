module Make = (
  T: {
    type op;
    type pre;
    type post;
    type bin;
    type t = HTerm.t(op, pre, post, bin);

    // TODO make this its own standalone intf (SortedTile)
    module Tile: {
      type op;
      type pre;
      type post;
      type bin;
      type t = Tile.t(op, pre, post, bin);

      let to_htile: t => HTile.t;
    };

    let to_htiles: t => HTile.s;
  },
  I: {
    type op;
    type pre;
    type post;
    type bin;
    type tile = Tile.t(op, pre, post, bin);
    type bidelimited = option(tile);
    type t =
      | Bidelimited(bidelimited)
      | Pre_r(T.pre, t)
      | Post_l(t, T.post)
      | Bin_l(t, T.bin, T.t)
      | Bin_r(T.t, T.bin, t)

    type parent;
    type child;

    let to_parent: t => parent;
    let to_child: t => child;

    let enter_tile: (~unzipped: t, Direction.t, T.Tile.t) => option((HTile.s, descendant));
  }
) => {
  /*
  let concat_opt = (unzipped: t', unzipped': t'): option(t') =>
    switch (unzipped) {
    | None => Some(unzipped')
    | Some(t') =>
      let concat = t => concat_opt(t, unzipped);
      switch (t') {
      | Op_z(Paren_body(t)) =>
        let+ t = concat(t);
        Some(Op_z(Paren_body(t)));
      | Pre_z(Let_def(p, t), body) =>
        let+ t = concat(t);
        Some(Pre_z(Let_def(p, t), body));
      | Pre_r(pre, t) =>
        let+ t = concat(t);
        Some(Pre_r(pre, t));
      | Post_l(t, post) =>
        let+ t = concat(t);
        Some(Post_l(t, post));
      | Post_z(fn, Ap_arg(t)) =>
        let+ t = concat(t);
        Some(Post_z(fn, Ap_arg(t)));
      | Bin_l(t, bin, r) =>
        let+ t = concat(t);
        Some(Bin_l(t, bin, r));
      | Bin_z(_, (), _) => raise(Void_bin)
      | Bin_r(l, bin, t) =>
        let+ t = concat(t);
        Some(Bin_l(l, bin, t));
      };
    };
  */

  let root = I.Bidelimited(None);

  let rec zip_to_nearest_bidelim = (
    zipped: HTile.s,
    unzipped: I.t
  ): (HTile.s, I.bidelimited) => {
    switch (unzipped) {
    | Bidelimited(bidelim) => (zipped, bidelim)
    | Pre_r(pre, unzipped) =>
      zip_to_nearest_bidelim([Tile.Pre(pre), ...zipped], unzipped)
    | Post_l(unzipped, post) =>
      zip_to_nearest_bidelim(zipped @ [Tile.Post(post)], unzipped)
    | Bin_l(unzipped, bin, r) =>
      zip_to_nearest_bidelim(zipped @ [Tile.Bin(bin), ...HExp.to_htiles(r)], unzipped)
    | Bin_r(l, bin, unzipped) =>
      zip_to_nearest_bidelim(HExp.to_htiles(l) @ [Tile.Bin(bin), ...zipped], unzipped)
    }
  };

  let enter =
      (
        d: Direction.t,
        {prefix, z, suffix}: ZList.t(T.Tile.t, T.Tile.t),
      )
      : option((HTile.s, I.descendant)) => {
    let n = List.length(prefix);
    let ts = prefix @ [z, ...suffix];
    let rec go = (~unzipped: t, skel: Skel.t) => {
      let t = List.nth(ts, Skel.root_index(skel));
      let enter = () => I.enter_tile(~unzipped, d, t);
      switch (skel) {
      | Op(m) =>
        assert(n == m);
        enter();
      | Pre(m, r) =>
        n == m
        ? enter()
        : go(~unzipped=Pre_r(Tile.get_pre(t), unzipped), r)
      | Post(l, m) =>
        n == m
        ? enter()
        : go(~unzipped=Post_l(unzipped, Tile.get_post(t)), l)
      | Bin(l, m, r) =>
        let bin = Tile.get_bin(t);
        if (n < m) {
          go(~unzipped=Bin_l(unzipped, bin, r), l);
        } else if (n > m) {
          go(~unzipped=Bin_r(l, bin, unzipped), r);
        } else {
          enter();
        };
      };
    };
    go(HExp.associate(ts));
  }
};

module rec Exp: EXP = {
  type t =
    | Bidelimited(bidelimited)
    | Pre_r(HExp.pre, t)
    | Post_l(t, HExp.post)
    | Bin_l(t, HExp.bin, HExp.t)
    | Bin_r(HExp.t, HExp.bin, t)
  and bidelimited = option(tile)
  and tile = Tile.t(op, pre, post, bin)
  and op =
    | Paren_body(t)
  and pre =
    | Let_def(HPat.t, t, HExp.t)
  and post =
    | Ap_arg(HExp.t, t)
  and bin = unit; // empty

  exception Void_bin;

  type child = [ | `Exp(t) | `Pat(Pat.t)];
  type child_tile = [ | `Exp(tile) | `Pat(Pat.tile) ];

  type parent = [ | `Exp(t) ];
  type parent_tile = [ | `Exp(tile) ];

  let exit_tile: (tile, HTile.s) => (HTile.s, parent) =
    Tile.get(
      fun
      | Paren_body(unzipped) =>
        z => ([Tile.Op(Paren(z))], `Exp(unzipped)),
      fun
      | Let_def(p, unzipped, body) =>
        z => ([Tile.Pre(Let(p, z)), ...HExp.to_htiles(body)], `Exp(unzipped)),
      fun
      | Ap_arg(fn, unzipped) =>
        z => (HExp.to_htiles(fn) @ [Tile.Post(Ap(z))], `Exp(unzipped)),
      fun
      | () => raise(Void_bin),
    );

  let move_to_parent_tile = (
    zipped: HTile.s,
  ): (tile => (HTile.s, parent)) =>
    Tile.get(
      fun
      | Paren_body(unzipped) =>
        ([Tile.Op(Paren(zipped))], `Exp(unzipped)),
      fun
      | Let_def(p, unzipped, body) =>
        ([Tile.Pre(Let(p, zipped)), ...HExp.to_htiles(body)], `Exp(unzipped)),
      fun
      | Ap_arg(fn, unzipped) =>
        (HExp.to_htiles(fn) @ [Tile.Post(Ap(zipped))], `Exp(unzipped)),
      fun
      | () => raise(Void_bin),
    );

  let move_to_next_child = (
    d: Direction.t,
    ts: HTile.s,
  ): (I.tile => option((HTile.s, I.descendant))) =>
    Tile.get(
      fun
      | Paren_body(_) => None,
      fun
      | Let_def(p, unzipped, body) =>
        switch (d) {
        | Right => None
        | Left =>
          let p = HPat.to_htiles(p);
          let def =
            HExp.mk(ts)
            |> OptUtil.get(() => raise(Invalid_argument("move_to_next_child")));
          let unzipped =
            `Pat(Tile.Pre(Pat.Let_pat(unzipped, def, body)));
          Some((p, unzipped));
        },
      fun
      | Ap_arg(_) => None,
      fun
      | () => raise(Void_bin),
    );

  let enter_tile = (~unzipped: t): (HExp.Tile.t => option((HTile.s, descendant))) =>
    Tile.get(
      fun
      | OpHole
      | Num(_)
      | Var(_) => None
      | Paren(body) => {
        let unzipped = Bidelim(Some(Op(Paren_body(unzipped))));
        Some((body, `Exp(unzipped)));
      }
      fun
      | Lam(p) => {
        let unzipped = Bidelim(Some(Pre(Lam_pat(unzipped))));
        Some((p, `Pat(unzipped)));
      }
      | Let(p, def) =>
        switch (d) {
        | Left =>
          let unzipped = Bidelim(Some(Pre(Let_def(p, unzipped))));
          Some((def, `Exp(unzipped)));
        | Right =>
          let unzipped = Bidelim(Some(Pre(Let_pat(unzipped, def))));
          Some((p, `Pat(unzipped)));
        },
      fun
      | Ap(arg) => {
        let unzipped = Bidelim(Some(Post(Ap_arg(unzipped))));
        Some((arg, `Exp(unzipped)));
      },
      () => raise(Void_bin),
    );
};

type t = [
  | `Exp(Exp.t)
  | `Pat(Pat.t)
  | `Typ(Typ.t)
];
