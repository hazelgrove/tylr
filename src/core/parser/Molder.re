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

module Melded = Melder.Melded;

let mold =
    (
      ~bound=Bound.Root,
      slope: Slope.Dn.t,
      ~fill=Cell.empty,
      t: Token.Unmolded.t,
    )
    : Melded.t =>
  switch (
    candidates(t)
    |> Oblig.Delta.minimize(tok =>
         Melder.push(~repair=true, tok, ~fill, slope, ~bound, ~onto=L)
       )
  ) {
  | Some(m) => m
  | None =>
    Melder.push(Token.Unmolded.defer(t), ~fill, slope, ~bound, ~onto=L)
    |> Options.get_fail("bug: failed to push space")
  };

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
      |> Option.map(((cell, (ts, cs))) =>
           Slope.Up.unroll(cell) @ [{...terr, wald: Wald.mk(ts, cs)}]
         )
      |> Option.value(~default=Slope.Up.unroll(terr.cell));
    (molded, Error(up));
  };
};
