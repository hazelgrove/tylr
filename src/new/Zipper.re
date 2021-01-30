module Make =
       (
         Zipped: {
           type tile;

           type normal = ZList.t(unit, tile);
           type selecting =
            ZList.t(
              (Direction.t, HSelection.t),
              Either.t(tile, HTessera.t),
            );
           type restructuring =
            ZList.t(
              (Direction.t, list(HSelection.t)),
              Either.t(tile, HSelection.t),
            );

           type t =
            | Normal(normal)
            | Selecting(selecting)
            | Restructuring(restructuring);
         },
         Unzipped: {
           type t;
           type tile;
           type bidelimited = option(tile);
         },
         I: {
           type t = (Zipped.tiles, Unzipped.bidelimited);

           // strict
           type parent;
           // non-strict
           type child;

           let to_child: t => child;

           let enter_tile:
             (~unzipped: Unzipped.t, Direction.t, Zipped.tile) =>
             option(child);
         },
       ) => {
  let rec zip_to_nearest_bidelim =
          (zipped: Zipped.tiles, unzipped: Unzipped.t): I.t => {
    switch (unzipped) {
    | Bidelimited(bidelim) => (zipped, bidelim)
    | Pre_r(pre, unzipped) =>
      zip_to_nearest_bidelim([Tile.Pre(pre), ...zipped], unzipped)
    | Post_l(unzipped, post) =>
      zip_to_nearest_bidelim(zipped @ [Tile.Post(post)], unzipped)
    | Bin_l(unzipped, bin, r) =>
      zip_to_nearest_bidelim(
        zipped @ [Tile.Bin(bin), ...HExp.to_htiles(r)],
        unzipped,
      )
    | Bin_r(l, bin, unzipped) =>
      zip_to_nearest_bidelim(
        HExp.to_htiles(l) @ [Tile.Bin(bin), ...zipped],
        unzipped,
      )
    };
  };

  let enter =
      (
        d: Direction.t,
        {prefix, z, suffix}: ZList.t(Zipped.tile, Zipped.tile),
        unzipped: Unzipped.t,
      )
      : option((int, child)) => {
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
        n == m ? enter() : go(~unzipped=Pre_r(Tile.get_pre(t), unzipped), r)
      | Post(l, m) =>
        n == m
          ? enter() : go(~unzipped=Post_l(unzipped, Tile.get_post(t)), l)
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
    go(~unzipped, HExp.associate(ts));
  };

  let select =


};

module rec Exp: EXP = {
  // TODO break apart Unzipped into separate modules
  type t = (HExp.tiles, Unzipped.Exp.bidelimited);

  type parent = [ | `Exp(t)];
  type child = [ | `Exp(t) | `Pat(Pat.t)];

  /*
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
   */

  let exit_tile =
      (d: Direction.t, zipped: Zipped.tiles, tile: Unzipped.Exp.tile)
      : (int, parent) => {
    let z = HExp.of_tiles(zipped);
    tile
    |> Tile.get(
         fun
         | Paren_body(unzipped) =>
           zip_to_nearest_bidelim([Tile.Op(HExp.Paren(z))], unzipped),
         fun
         | Let_def(p, unzipped, body) => (
             [Tile.Pre(Let(p, z)), ...HExp.to_tiles(body)],
             unzipped,
           ),
         fun
         | Ap_arg(fn, unzipped) => (
             HExp.to_tiles(fn) @ [Tile.Post(Ap(z))],
             unzipped,
           ),
         fun
         | () => raise(Void_bin),
       );
  };

  let enter_tile =
      (d: Direction.t, tile: HExp.tile, unzipped: Unzipped.Exp.t)
      : option(child) =>
    tile
    |> Tile.get(
         fun
         | OpHole
         | Num(_)
         | Var(_) => None
         | Paren(body) => {
             let unzipped = Bidelim(Some(Op(Paren_body(unzipped))));
             Some((body, `Exp(unzipped)));
           },
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
         () =>
         raise(Void_bin)
       );

  include Make(HExp, Unzipped.Exp, Exp);
}
and Pat: PAT = {
  // todo
}
and Typ: TYP = {
  // todo
};

type t = [ | `Exp(Exp.t) | `Pat(Pat.t) | `Typ(Typ.t)];

let apply = (f: (module S, S.t) => 'a, zipper: t): 'a =>
  switch (zipper) {
  | `Exp(zipper) => f((module Exp), zipper)
  | `Pat(zipper) => f((module Pat), zipper)
  | `Typ(zipper) => f((module Typ), zipper)
  };

let move = (
  d: Direction.t,
  (j, ()): EditMode.normal,
  zipper: t,
): option((EditMode.normal, t)) => {
  // TODO define signature S
  let go =
      ((module S: S), (zipped, unzipped): S.t)
      : option((EditMode.normal, Zipper.t)) => {
    let n = d == Left ? j - 1 : j;
    if (n < 0 || n >= List.length(zipped)) {
      let+ unzipped_tile = unzipped;
      // TODO first try moving to next child
      let+ (j, parent) = S.exit_tile(d, zipped, unzipped_tile);
      ((j, ()), parent :> Zipper.t);
    } else {
      switch (enter(d, ZList.split_at(n, zipped), Bidelimited(unzipped))) {
      | Some((j, child)) => Some((j, R(child)))
      | None =>
        let j = d == Left ? j - 1 : j + 1;
        Some(((j, ()), S.to_child(zipper) :> Zipper.t));
      };
    };
  };
  Zipper.apply(go, zipper);
};

let select = (d: Direction.t, )