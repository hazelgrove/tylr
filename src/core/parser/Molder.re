open Stds;

// replace ghost with piece above bridge
// let x = 1 >in< x + 1
// let x = 1 >in< x + 1 [in]
// let x = 1 >< x + 1 [in]
// let x = 1 >< x + 1 in <>

// replace ghost with piece under bridge
// let x = 1 + 2 >in< x + 1
// let x = 1 [in] + 2 >in< x + 1
//
// let x = 1 in <> + 2 >< x + 1

// replacing even solid bridges?
// let x = 1 + 2 in x + 1
// let x = 1 [in] + 2 in x + 1
//
// let x = 1 in <> + 2 >in< x + 1
// or
// let x = 1 in <> + 2 >< <in> >< x + 1

let candidates = (t: Token.Unmolded.t): list(Token.t) =>
  List.map(
    Token.mk(~id=t.id, ~text=t.text),
    switch (t.mtrl) {
    | Space () => [Mtrl.Space()]
    | Grout(_) => failwith("bug: attempted to mold grout")
    | Tile(lbls) =>
      lbls
      |> List.concat_map(lbl =>
           Molds.with_label(lbl) |> List.map(mold => Mtrl.Tile((lbl, mold)))
         )
    },
  );

let mold =
    (
      ~bound=Bound.Root,
      slope: Slope.Dn.t,
      ~fill=Cell.empty,
      t: Token.Unmolded.t,
    )
    : Rel.t(Terr.R.t, Slope.Dn.t) =>
  candidates(t)
  |> List.map(Wald.of_tok)
  |> Oblig.Delta.minimize(tok =>
       Melder.push_bounded(tok, ~fill, slope, ~bound, ~onto=L)
     )
  |> Option.value(
       ~default=
         Rel.Neq(
           Melder.push_space(Token.Unmolded.defer(t), slope, ~onto=L),
         ),
     );

let remold =
    (~bound=Bound.Root, slope: Slope.Dn.t, ~fill=Cell.empty, terr: Terr.L.t)
    : (Melded.t, Result.t(Cell.t, Slope.Up.t)) => {
  let (hd, tl) = Wald.uncons(terr.wald);
  let molded = mold(~bound, slope, ~fill, Token.unmold(hd));
  // fast path for when hd retains original mold
  if (Melded.face(molded) == hd.mtrl) {
    (Melded.extend(tl, molded), Ok(terr.cell));
  } else {
    let up =
      Chain.Affix.uncons(tl)
      |> Option.map((cell, (ts, cs)) =>
           Slope.Up.unroll(cell) @ [{...terr, wald: Wald.mk(ts, cs)}]
         )
      |> Option.value(~default=Slope.Up.unroll(terr.cell));
    (molded, Error(up));
  };
};

let rec remold = (~fill=Cell.empty, ctx: Ctx.t): (Cell.t, Ctx.t) => {
  let ((dn, up), tl) = Ctx.split_hd(ctx);
  switch (Slope.unlink(up)) {
  | Some((tok, cell, up)) when Token.Grout.is(tok) =>
    Effects.remove(tok);
    let up = Slope.cat(Slope.Up.unroll(cell), up);
    remold(~fill, Ctx.zip((dn, up), ~suf=tl));
  | _ =>
    switch (up) {
    | [] =>
      let cell = Melder.complete_slope(~onto=L, dn, ~fill);
      let ctx = Ctx.zip(Frame.Open.empty, ~suf=tl);
      (cell, ctx);
    | [terr, ...up] =>
      let ctx = Ctx.put_hd((dn, up), ctx);
      let (hd, rest) = Wald.split_hd(terr.wald);
      let molded = mold(ctx, ~fill, Token.Unmolded.unmold(hd));
      switch (Ctx.face(~side=L, molded)) {
      | Some(mtrl) when mtrl == hd.mtrl =>
        // fast path for when face piece retains mold
        molded
        |> Ctx.map_hd(Frame.Open.extend(~side=L, rest))
        |> remold(~fill=terr.cell)
      | _ =>
        // otherwise add rest of wald to suffix queue
        let up =
          switch (rest) {
          | ([], _) => []
          | ([cell, ...cells], toks) =>
            let terr = {...terr, wald: Wald.mk(toks, cells)};
            let _ = failwith("todo: make sure cell distributes paths");
            Slope.cat(Melder.Slope.Up.unroll(cell), [terr]);
          };
        ctx |> Ctx.map_hd(Frame.Open.cat(([], up))) |> remold;
      };
    }
  };
};
