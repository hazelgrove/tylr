open Stds;

// module Melded = {
//   type t = Rel.t(Wald.t, Slope.t);

//   let mk_eq = (_src: Wald.t, _bake, _dst: Wald.t) =>
//     failwith("todo Melded mk_eq");
//   let mk_neq = (_bake: Baked.t, _dst: Wald.t) =>
//     failwith("todo Melded mk_neq");
//   let mk = (src: Wald.t, bake: Baked.t, dst: Wald.t) =>
//     switch (Baked.is_eq(bake)) {
//     | Some(eq) => Rel.Eq(mk_eq(src, eq, dst))
//     | None => Rel.Neq(mk_neq(bake, dst))
//     };
// };

module Melded = {
  type t = (Slope.t, Wald.t);
  let map_hd = (f: Wald.t => Wald.t, (s, w): t) =>
    switch (s) {
    | [] => ([], f(w))
    | [hd, ...tl] => ([{...hd, wald: f(hd.wald)}, ...tl], w)
    };
  let link = (w: Wald.t, c: Rel.t(Cell.t, Cell.t), (slope, wald): t) =>
    switch (c) {
    | Neq(c) => ([Terr.{cell: c, wald: Wald.rev(w)}, ...slope], wald)
    | Eq(c) => map_hd(Wald.zip_cell(w, c), (slope, wald))
    };
  let mk = (src: Wald.t, baked: Baked.t, dst: Wald.t) =>
    baked
    |> Baked.fold(
         c => link(dst, c),
         (t, c) => link(Wald.of_tok(t), c),
         (Slope.empty, src),
       );
};

module W = Wald;
module Wald = {
  let lt = (l: W.t, r: W.t) =>
    !Walk.Set.is_empty(Walker.lt(Node(W.face(l)), Node(W.face(r))));
  let gt = (l: W.t, r: W.t) =>
    !Walk.Set.is_empty(Walker.gt(Node(W.face(l)), Node(W.face(r))));
  let eq = (l: W.t, r: W.t) =>
    !Walk.Set.is_empty(Walker.eq(Node(W.face(l)), Node(W.face(r))));

  let attach = (wald: W.t, baked: Baked.t): Terr.t =>
    baked
    |> Chain.map_loop(
         fun
         | Rel.Neq(_) => failwith("bug: expected only eq strides")
         | Eq(c) => c,
       )
    |> Chain.fold_right(
         (c, t, (cell, wald)) => (c, W.link(t, cell, wald)),
         c => (c, wald),
       )
    |> (((cell, wald)) => Terr.{wald: W.rev(wald), cell});

  // assumes w is already oriented toward side.
  // used to complete zigg top when it takes precedence over pushed wald.
  let round = (~side: Dir.t, ~fill=Fill.empty, w: W.t): Terr.t => {
    let bake = Baker.bake(~from=Dir.toggle(side));
    let exited = Walker.exit(~from=Dir.toggle(side), Node(W.face(w)));
    switch (Oblig.Delta.minimize(bake(~fill), Walk.Set.elements(exited))) {
    | Some(baked) => attach(w, baked)
    | None =>
      if (!Fill.is_empty(fill)) {
        print_endline("warning: dropping fill " ++ Fill.show(fill));
      };
      let exited =
        Walk.Set.min_elt_opt(exited)
        |> Options.get_fail("bug: expected at least one exit");
      let baked =
        bake(exited)
        |> Options.get_fail(
             "bug: bake expected to succeed if no fill required",
           );
      attach(w, baked);
    };
  };

  let meld =
      (~repair=false, ~from: Dir.t, src: W.t, ~fill=Fill.empty, dst: W.t)
      : option(Melded.t) => {
    open Options.Syntax;
    let walk = repair ? Walker.walk : Walker.step;
    let rec go = (src, fill) => {
      let/ () = repair ? rm_ghost_and_go(src, fill) : None;
      let+ bake =
        walk(~from, Node(W.face(src)), Node(W.face(dst)))
        |> Walk.Set.elements
        |> Oblig.Delta.minimize(~to_zero=!repair, Baker.bake(~from, ~fill));
      Melded.mk(src, bake, dst);
    }
    and rm_ghost_and_go = (src, fill) =>
      switch (W.unlink(src)) {
      | Ok((hd, cell, tl)) when Option.is_some(Token.Tile.is_ghost(hd)) =>
        let fill = Fill.cons(cell, fill);
        switch (go(tl, fill)) {
        // require eq match further in to accept removing hd
        | Some(([], _)) as r =>
          Effects.remove(hd);
          r;
        | _ => None
        };
      | _ => None
      };

    // first try zipping
    let/ () = {
      let caret = Fill.is_caret(fill);
      let+ zipped = W.zip_hds(~from, src, ~caret?, dst);
      // assert(Fill.is_empty(fill));
      ([], zipped);
    };
    go(src, fill);
  };

  let meld_root =
      (~repair=false, ~from: Dir.t, ~fill=Fill.empty, dst: W.t)
      : option(Slope.t) =>
    Walker.walk(~from, Root, Node(W.face(dst)))
    |> Walk.Set.elements
    |> Oblig.Delta.minimize(~to_zero=!repair, Baker.bake(~from, ~fill))
    |> Option.map(
         Baked.fold(
           c => Slope.link(dst, c),
           (t, c) => Slope.link(Wald.of_tok(t), c),
           Slope.empty,
         ),
       );
};

module T = Terr;
module Terr = {
  let attach = (baked: Baked.t, terr: T.t) =>
    baked
    |> Chain.map_loop(
         fun
         | Rel.Neq(_) => failwith("bug: expected only eq strides")
         | Eq(c) => c,
       )
    |> Chain.fold_right(
         (cell, tok) => Meld.link(~cell, tok),
         cell => Meld.M(cell, terr.wald, terr.cell),
       );

  let roll = (~onto: Dir.t, ~fill=Fill.empty, terr: T.t): Fill.t => {
    let bake = Baker.bake(~from=onto);
    let exited = Walker.exit(~from=onto, Node(T.face(terr)));
    let orient = Dir.pick(onto, (Meld.rev, Fun.id));
    switch (Oblig.Delta.minimize(bake(~fill), Walk.Set.elements(exited))) {
    | Some(baked) => Fill.unit(Cell.put(orient(attach(baked, terr))))
    | None =>
      let exited =
        Walk.Set.min_elt_opt(exited)
        |> Options.get_fail("bug: expected at least one exit");
      let baked =
        bake(exited)
        |> Options.get_fail(
             "bug: bake expected to succeed if no fill required",
           );
      Fill.cons(Cell.put(orient(attach(baked, terr))), fill);
    };
  };

  module L = {
    let roll = roll(~onto=R);
  };
  module R = {
    let roll = roll(~onto=L);
  };
};

module S = Slope;
module Slope = {
  let unroll = (~from: Dir.t, cell: Cell.t) => {
    let rec go = (cell: Cell.t, unrolled) =>
      switch (Cell.get(cell)) {
      | None => unrolled
      | Some(M(l, w, r)) =>
        let (cell, terr) =
          switch (from) {
          | L => (r, T.{wald: W.rev(w), cell: l})
          | R => (l, T.{wald: w, cell: r})
          };
        go(cell, [terr, ...unrolled]);
      };
    go(cell, []);
  };

  let roll = (~onto: Dir.t, ~fill=Fill.empty) =>
    S.fold(fill => Terr.roll(~onto, ~fill), fill);
  // let roll_fail =

  let push =
      (~repair=false, ~onto: Dir.t, w: W.t, ~fill=Fill.empty, slope: S.t)
      : Result.t(S.t, Fill.t) => {
    let meld = Wald.meld(~repair, ~from=onto);
    let roll = Terr.roll(~onto);
    let rec go = (fill, slope: S.t) =>
      switch (slope) {
      | [] => Error(fill)
      | [{wald: W(([tok, ...toks], cells)), cell}, ...tl]
          when Token.Grout.is(tok) =>
        Effects.remove(tok);
        let (cell, slope) =
          switch (cells) {
          | [] => (cell, tl)
          | [c, ...cs] =>
            let hd = T.{wald: W.mk(toks, cs), cell};
            (c, [hd, ...tl]);
          };
        go(fill, S.cat(unroll(~from=onto, cell), slope));
      | [hd, ...tl] =>
        switch (meld(hd.wald, ~fill, w)) {
        | None => go(roll(~fill, hd), tl)
        | Some(([], wald)) => Ok([{...hd, wald}, ...tl])
        | Some(([_, ..._] as s, wald)) =>
          let slope = [{...hd, wald}, ...tl];
          Ok(S.cat(s, slope));
        }
      };
    go(fill, slope);
  };

  // here "from" indicates which side slope is relative to puller
  // eg "pull from dn slope on left"
  let pull = (~from: Dir.t, slope: S.t): option((Token.t, S.t)) =>
    switch (slope) {
    | [] => None
    | [hd, ...tl] =>
      let (tok, rest) = W.split_hd(hd.wald);
      let slope =
        switch (rest) {
        | ([], _) => S.cat(unroll(~from, hd.cell), tl)
        | ([cell, ...cells], toks) =>
          let hd = {...hd, wald: W.mk(toks, cells)};
          S.cat(unroll(~from, cell), [hd, ...tl]);
        };
      Some((tok, slope));
    };

  module Dn = {
    let unroll = unroll(~from=L);
    let roll = roll(~onto=L);
    let push = push(~onto=L);
    let pull = pull(~from=L);
  };
  module Up = {
    let unroll = unroll(~from=R);
    let roll = roll(~onto=R);
    let push = push(~onto=R);
    let pull = pull(~from=R);
  };
};
