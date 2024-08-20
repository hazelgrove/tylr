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
    Token.mk(~id=t.id, ~marks=?t.marks, ~text=t.text),
    switch (t.mtrl) {
    | Space(t) => [Mtrl.Space(t)]
    | Grout(_) => failwith("bug: attempted to mold grout")
    | Tile(lbls) =>
      lbls
      |> List.concat_map(lbl =>
           Molds.with_label(lbl) |> List.map(mold => Mtrl.Tile((lbl, mold)))
         )
    },
  );

module Melded = Melder.Melded;

module Molded = {
  type t =
    | Molded(Melded.t)
    | Deferred(Melded.t)
    // removed redundant empty ghost
    | Removed;
};

// returns None if input token is empty
let mold =
    (
      ~bound=Bound.Root,
      slope: Slope.Dn.t,
      ~fill=Cell.empty,
      t: Token.Unmolded.t,
    )
    : Molded.t =>
  switch (
    candidates(t)
    |> Oblig.Delta.minimize(tok =>
         Melder.push(~repair=true, tok, ~fill, slope, ~bound, ~onto=L)
       )
  ) {
  // pushed token was empty ghost connected via neq-relation
  // (note: Neq constructor name misleading here, indicates neq/eq wrt slope bound)
  | Some(Neq([{wald: W(([face], [])), _}, ..._]))
      when Mtrl.is_tile(face.mtrl) && face.text == "" =>
    Removed
  | Some(m) => Molded(m)
  | None =>
    let deferred = Token.Unmolded.defer(t);
    Token.is_empty(deferred)
      ? Removed
      : Deferred(
          Melder.push(deferred, ~fill, slope, ~bound, ~onto=L)
          |> Options.get_fail("bug: failed to push space"),
        );
  //
  };

// returns the result of remolding and melding the terr face onto bounded slope.
// if the terr face retains its original mold, then the rest of the terr is tacked
// on and snd elem of returned pair is Ok(terr.cell). otherwise, the rest of the
// terr is disassembled to an up slope that requires subsequent remolding.
// note: if returns (o, Ok(cell)), then o must be Some
// todo: clean this up
let remold =
    (~bound=Bound.Root, slope: Slope.Dn.t, ~fill=Cell.empty, terr: Terr.L.t)
    : (Molded.t, Result.t(Cell.t, Slope.Up.t)) => {
  let (hd, tl) = Wald.uncons(terr.wald);
  switch (mold(~bound, slope, ~fill, Token.unmold(hd))) {
  | Molded(m) when Melded.face(m).mtrl == hd.mtrl =>
    // fast path for when hd retains original mold
    (Molded(Melded.extend(tl, m)), Ok(terr.cell))
  | molded =>
    let up =
      Chain.Affix.uncons(tl)
      |> Option.map(((cell, (ts, cs))) =>
           Slope.Up.unroll(cell) @ [{...terr, wald: Wald.mk(ts, cs)}]
         )
      |> Option.value(~default=Slope.Up.unroll(terr.cell));
    (molded, Error(up));
  };
};
