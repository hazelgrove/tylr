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
    | Grout(_) => []
    | Tile(lbls) =>
      lbls
      |> List.concat_map(lbl =>
           Molds.with_label(lbl) |> List.map(mold => (lbl, mold))
         )
      |> List.stable_sort(((lbl_l, m_l: Mold.t), (lbl_r, m_r: Mold.t)) => {
           open Compare.Syntax;
           let/ () = Sort.compare(m_l.sort, m_r.sort);
           (-1)
           * Bool.compare(
               Label.is_complete(t.text, lbl_l),
               Label.is_complete(t.text, lbl_r),
             );
         })
      |> List.map(Mtrl.tile)
    },
  );

let complete_pending_ghosts = (~bounds, l: Stack.t, ~fill) => {
  let (cell, effs) =
    Effects.dry_run(
      () => Melder.complete_bounded(~bounds, ~onto=L, l.slope, ~fill),
      (),
    );
  // hack(?) to avoid completion if no new ghosts are generated. if only
  // grout are generated, then we can generate them later as needed at the
  // end of remolding. better to delay their generation bc there may already
  // be grout in the suffix whose relative position around neighboring
  // whitespace we want to preserve.
  effs
  |> List.for_all(
       fun
       | Effects.Insert(tok) =>
         Mtrl.(is_grout(tok.mtrl) || is_space(tok.mtrl))
       | Remove(_) => true,
     )
    ? (l, fill)
    : {
      Effects.commit(effs);
      let (fill, slope) = Slope.Dn.unroll(cell);
      ({...l, slope}, fill);
    };
};

// returns Error(fill) if input token is empty
// re indicates whether token is being remolded
let rec mold =
        (~re=false, stack: Stack.t, ~fill=Cell.empty, t: Token.Unmolded.t)
        : Result.t((Token.t, Grouted.t, Stack.t), Cell.t) => {
  // P.log("--- Molder.mold");
  // P.show("re", string_of_bool(re));
  // P.show("stack", Stack.show(stack));
  // P.show("fill", Cell.show(fill));
  // P.show("t", Token.Unmolded.show(t));
  switch (
    candidates(t)
    |> Oblig.Delta.minimize(tok =>
         Melder.push(~no_eq=re, tok, ~fill, stack, ~onto=L, ~repair=remold)
         |> Option.map(((grouted, stack)) => (tok, grouted, stack))
       )
  ) {
  | Some((tok, grouted, stack) as molded) =>
    // remove empty ghost connected via neq-relation
    // P.log("--- Molder.mold/success");
    // P.show("tok", Token.show(tok));
    // P.show("grouted", Grouted.show(grouted));
    // P.show("stack", Stack.show(stack));
    Mtrl.is_tile(tok.mtrl)
    && Token.is_empty(tok)
    && (
      Grouted.is_neq(grouted)
      || Option.is_some(Grouted.is_eq(grouted))
      && stack.slope == []
    )
      ? Error(Cell.mark_degrouted(fill, ~side=R)) : Ok(molded)
  | None =>
    let deferred = Token.Unmolded.defer(t);
    Token.is_empty(deferred)
      ? Error(Cell.mark_degrouted(fill, ~side=R))
      : Ok(
          {
            let (fill, slope) = Slope.Dn.unroll(fill);
            let stack = Stack.cat(slope, stack);
            Melder.push(~no_eq=re, deferred, ~fill, stack, ~onto=L)
            |> Option.map(((grouted, stack)) => (deferred, grouted, stack))
            |> Options.get_fail("bug: failed to push space");
          },
        );
  };
}
// returns Ok if all suffix elements are remolded by the prefix without changing
// stack bound. returns Error if a suffix element changes stack bound when remolded
// and returns remaining suffix elements.
and remold =
    (~fill, (l, r): Stack.Frame.t)
    : Result.t((Slope.Dn.t, Cell.t), (Cell.t, Stack.Frame.t)) => {
  // P.log("--- Molder.remold");
  // P.show("fill", Cell.show(fill));
  // P.show("(l, r)", Stack.Frame.show((l, r)));
  let bounds = (l.bound, r.bound);
  switch (r.slope) {
  | [] =>
    // P.log("--- Molder.remold/done");
    // P.show("l", Stack.show(l));
    // P.show("fill", Cell.show(fill));
    Ok((l.slope, fill))
  | [hd, ...tl] =>
    // P.log("--- Molder.remold/continue");
    // P.show("hd", Terr.show(hd));
    // insert any pending ghosts if next terr has newlines
    let (l, fill) =
      Terr.tokens(hd)
      |> List.map(Token.height)
      |> List.fold_left((+), 0) == 0
        ? (l, fill) : complete_pending_ghosts(~bounds, l, ~fill);
    let r_tl = {...r, slope: tl};
    let (hd_w, tl_w) = Wald.uncons(hd.wald);
    // tl_w unrolled to up slope
    let unroll_tl_w_hd_cell = () =>
      Chain.Affix.uncons(tl_w)
      |> Option.map(((cell, (ts, cs))) => {
           let (c, up) = Slope.Up.unroll(cell);
           let up = up @ [{wald: Wald.mk(ts, cs), cell: hd.cell}];
           (c, up);
         })
      |> Option.value(~default=Slope.Up.unroll(hd.cell));
    // P.log("--- Molder.remold/continue/molding");
    // P.show("l", Stack.show(l));
    // P.show("fill", Cell.show(fill));
    // P.show("hd_w", Token.show(hd_w));
    switch (mold(~re=true, l, ~fill, Token.unmold(hd_w))) {
    | Error(fill) =>
      Effects.remove(hd_w);
      let (c, up) = unroll_tl_w_hd_cell();
      let fill = fill |> Cell.pad(~r=c) |> Cell.mark_ends_dirty;
      (l, r_tl) |> Stack.Frame.cat(([], up)) |> remold(~fill);
    | Ok((t, grouted, rest)) when t.mtrl == hd_w.mtrl =>
      // fast path for when hd_w retains original meld
      // P.log("--- Molder.remold/continue/fast path");
      // P.show("t", Token.show(t));
      // P.show("grouted", Grouted.show(grouted));
      // P.show("rest", Stack.show(rest));
      let connected = Stack.connect(t, grouted, rest) |> Stack.extend(tl_w);
      let fill = Cell.mark_ends_dirty(hd.cell);
      if (connected.bound == l.bound) {
        remold(~fill, (connected, r_tl));
      } else {
        Error((fill, (connected, r_tl)));
      };
    | Ok((t, grouted, rest)) =>
      Effects.remove(hd_w);
      let connected = Stack.connect(Effects.insert(t), grouted, rest);
      // check if connection changed the stack bound
      if (connected.bound == l.bound) {
        // if not, then nearest bidelimited container is preserved
        let (fill, up) = unroll_tl_w_hd_cell();
        (connected, r_tl)
        |> Stack.Frame.cat(([], up))
        |> remold(~fill=Cell.mark_ends_dirty(fill));
      } else {
        Error((Cell.mark_ends_dirty(hd.cell), (connected, r_tl)));
      };
    };
  };
};
