open Util;

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

let mold = (ctx: Ctx.t, ~fill=Fill.empty, t: Token.Unmolded.t) =>
  switch (
    candidates(t)
    |> Oblig.Delta.minimize(tok => Melder.Ctx.push(~onto=L, tok, ~fill, ctx))
  ) {
  | Some(ctx) => ctx
  | None =>
    ctx
    |> Melder.Ctx.push(~onto=L, Token.Unmolded.defer(t))
    |> Options.get_fail("bug: failed to meld unmolded token")
  };

let rec remold = (~fill=Fill.empty, ctx: Ctx.t) => {
  let ((dn, up), tl) = Ctx.split_hd(ctx);
  switch (up) {
  | [] =>
    let unrolled =
      Fill.to_list(fill)
      |> List.rev_map(Melder.Slope.Dn.unroll)
      |> List.concat;
    Ctx.zip((Slope.cat(unrolled, dn), []), ~suf=tl);
  | [terr, ...up] when Mtrl.is_grout(Terr.sort(terr)) =>
    let unrolled =
      Terr.cells(terr) |> List.concat_map(Melder.Slope.Up.unroll);
    remold(Ctx.zip((dn, Slope.cat(unrolled, up)), ~suf=tl));
  | [terr, ...up] =>
    let ctx = Ctx.put_hd((dn, up), ctx);
    let (hd, rest) = Wald.split_hd(terr.wald);
    let molded = mold(ctx, ~fill, Token.Unmolded.unmold(hd));
    switch (Ctx.face(~side=L, molded)) {
    | Some(mtrl) when mtrl == hd.mtrl =>
      // fast path for when face piece retains mold
      molded
      |> Ctx.extend(~side=L, rest)
      |> Option.get  // must succeed if Ctx.face succeeded
      |> remold(~fill=Fill.unit(terr.cell))
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
  };
};
