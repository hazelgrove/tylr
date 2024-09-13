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

// returns None if input token is empty
let mold =
    (stack: Stack.t, ~fill=Cell.empty, t: Token.Unmolded.t)
    : option((Token.t, Grouted.t, Stack.t)) =>
  switch (
    candidates(t)
    |> Oblig.Delta.minimize(tok =>
         Melder.push(~repair=true, tok, ~fill, stack, ~onto=L)
         |> Option.map(((grouted, stack)) => (tok, grouted, stack))
       )
  ) {
  // pushed token was empty ghost connected via neq-relation
  | Some((tok, grouted, _) as molded) =>
    Mtrl.is_tile(tok.mtrl) && tok.text == "" && Grouted.is_neq(grouted)
      ? None : Some(molded)
  | None =>
    let deferred = Token.Unmolded.defer(t);
    Token.is_empty(deferred)
      ? None
      : Some(
          Melder.push(deferred, ~fill, stack, ~onto=L)
          |> Option.map(((grouted, stack)) => (deferred, grouted, stack))
          |> Options.get_fail("bug: failed to push space"),
        );
  };
