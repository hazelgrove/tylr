module Make =
       (
         G: {
           // global
           type t;
           type normal;
         },
         T: {
           type tile;
           let mk_tile:
             (HTessera.open_, list(tile), HTessera.close) => option(tile);
         },
         Zipped: {
           type normal = ZList.t(unit, T.tile);
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
           type t = (Zipped.t, Unzipped.bidelimited);
           type normal = (Zipped.normal, Unzipped.bidelimited);

           let mk_g: t => G.t;

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
  let restructuring_of_normal =
      (
        restructuring: (Direction.t, list(HSelection.t)),
        {prefix, z: (), suffix}: Zipped.normal,
      )
      : Zipped.restructuring => {
    let prefix = List.map(Either.l, prefix);
    let suffix = List.map(Either.l, suffix);
    {prefix, z: restructuring, suffix};
  };

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

  let move_normal =
      (
        d: Direction.t,
        {prefix, z: (), suffix}: Zipped.normal,
        unzipped: Unzipped.bidelimited,
      )
      : option(G.normal) => {
    let j = List.length(prefix);
    let exit = () => {
      let+ unzipped_tile = unzipped;
      // TODO first try moving to next child
      I.exit_tile(d, prefix @ suffix, unzipped_tile);
    };
    switch (d) {
    | Left when j == 0 => exit()
    | Right when j == List.length(zipped) => exit()
    | _ =>
      let zipped = prefix @ suffix;
      let n = d == Left ? j - 1 : j;
      switch (enter(d, ZList.split_at(n, zipped), Bidelimited(unzipped))) {
      | Some(_) as entered => entered
      | None =>
        let j = d == Left ? j - 1 : j + 1;
        let (prefix, suffix) = ListUtil.split_n(j, zipped);
        Some(I.mk_g_normal(({prefix, z: (), suffix}, unzipped)));
      };
    };
  };

  let rec move_selecting =
          (
            d: Direction.t,
            {prefix, z: (side, selection), suffix}: Zipped.selecting,
            unzipped: Unzipped.bidelimited,
          )
          : option(G.t) => {
    let moved_within_zipped = (~prefix=prefix, ~suffix=suffix, selection) =>
      Some(I.mk_g(({prefix, z: (side, selection), suffix}, unzipped)));
    if (d != side) {
      if (selection == []) {
        move_selecting(d, {prefix, z: (d, selection), suffix}, unzipped);
      } else {
        switch (side) {
        | Left =>
          let (first, trailing) = ListUtil.split_first(selection);
          switch (first) {
          | R(_) =>
            let prefix =
              HSelection.parse(~mk_tile=T.mk_tile, prefix @ [first]);
            moved_within_zipped(~prefix, trailing);
          | L(tile) =>
            let (open_, body, close) = HTile.flatten_tile(tile);
            let prefix = prefix @ [R(open_)];
            let selection = body @ [R(close), ...trailing];
            moved_within_zipped(~prefix, selection);
          };
        | Right =>
          let (leading, last) = ListUtil.split_last(selection);
          switch (last) {
          | R(_) =>
            let suffix =
              HSelection.parse(~mk_tile=T.mk_tile, [last, ...suffix]);
            moved_within_zipped(~suffix, selection);
          | L(tile) =>
            let (open_, body, close) = HTile.flatten_tile(tile);
            let suffix = [R(close), ...suffix];
            let selection = leading @ [R(open_), ...body];
            moved_within_zipped(~suffix, selection);
          };
        };
      };
    } else {
      switch (side) {
      | Left =>
        switch (ListUtil.split_last_opt(prefix)) {
        | None => failwith("todo: move into unzipped")
        | Some((leading, R(tessera))) =>
          let selection =
            HSelection.parse(
              ~mk_tile=T.mk_tile,
              [R(tessera), ...selection],
            );
          moved_within_zipped(~suffix=leading, selection);
        | Some((leading, L(tile))) =>
          let (open_, body, close) = T.flatten_tile(tile);
          let prefix =
            prefix
            @ [R(Open(open_)), ...List.map(Either.l, T.flatten(body))];
          let selection = [R(Close(close)), ...selection];
          moved_within_zipped(~prefix, selection);
        }
      | Right =>
        switch (suffix) {
        | [] => failwith("todo: move into unzipped")
        | [R(tessera), ...trailing] =>
          let selection =
            HSelection.parse(~mk_tile=T.mk_tile, selection @ [R(tessera)]);
          moved_within_zipped(~suffix=trailing, selection);
        | [L(tile), ...trailing] =>
          let (open_, body, close) = T.flatten_tile(tile);
          let suffix =
            List.map(Either.l, T.flatten(body))
            @ [R(Close(close)), ...suffix];
          let selection = selection @ [R(Open(open_))];
          moved_within_zipped(~suffix, selection);
        }
      };
    };
  };

  let move_restructuring =
      (
        d: Direction.t,
        {prefix, z, suffix}: Zipped.restructuring,
        unzipped: Unzipped.bidelimited,
      )
      : option(G.t) => {
    let (side, selections) = z;
    let picked_up_selection = (~prefix=prefix, ~suffix=suffix, selections) =>
      Some(I.mk_g(({prefix, z: selections, suffix}, unzipped)));
    let picked_up_all_selections =
      List.for_all(Either.is_L, prefix) && List.for_all(Either.is_L, suffix);
    let move_through_tile = (prefix, tile, suffix) =>
      if (picked_up_all_selections) {
        let prefix = List.filter_map(Either.get_L, prefix);
        let suffix = List.filter_map(Either.get_L, suffix);
        let+ normal = move_normal(d, {prefix, z: (), suffix}, unzipped);
        G.restructuring_of_normal(normal);
      } else {
        let (prefix, suffix) =
          switch (d) {
          | Left => (prefix, [tile, ...suffix])
          | Right => (prefix @ [tile], suffix)
          };
        Some(I.mk_g({prefix, z, suffix}, unzipped));
      };
    switch (d) {
    | Left =>
      switch (ListUtil.split_last_opt(prefix)) {
      | None => failwith("todo")
      | Some((leading, L(_) as tile)) =>
        move_through_tile(leading, tile, suffix)
      | Some((leading, R(_) as selection)) =>
        picked_up_selection(
          ~prefix=leading,
          (Left, [selection, ...selections]),
        )
      }
    | Right =>
      switch (suffix) {
      | [] => failwith("todo")
      | [L(_) as tile, ...trailing] =>
        move_through_tile(prefix, tile, suffix)
      | [R(selection), ...trailing] =>
        picked_up_selection(
          ~suffix=trailing,
          (Right, selections @ [selection]),
        )
      }
    };
  };

  let move = (d: Direction.t, (zipped, unzipped): t): option(G.t) =>
    switch (zipped) {
    | Normal(normal) => move_normal(d, normal, unzipped)
    | Selecting(selecting) => move_selecting(d, selecting, unzipped)
    | Restructuring(restructuring) =>
      move_restructuring(d, restructuring, unzipped)
    };
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
}
and G: G = {
  type t = [ | `Exp(Exp.t) | `Pat(Pat.t) | `Typ(Typ.t)];

  let apply = (f: ((module S), S.t) => 'a, zipper: t): 'a =>
    switch (zipper) {
    | `Exp(zipper) => f((module Exp), zipper)
    | `Pat(zipper) => f((module Pat), zipper)
    | `Typ(zipper) => f((module Typ), zipper)
    };

  let move = (d: Direction.t, zipper: t): option(t) =>
    switch (zipper) {
    | `Exp(z) => Exp.move(d, z)
    | `Pat(z) => Pat.move(d, z)
    | `Typ(z) => Typ.move(d, z)
    };
};
include G;
