let pad_wrap = (c: Cell.t) => {
  let w = Wald.of_tok(Token.mk(~text=" ", Mtrl.Space(White(Sys))));
  let m = {
    // this choice only matters when c has caret
    let (l, r) = Meld.(mk(~l=c, w), mk(w, ~r=c));
    switch (Mode.get()) {
    | Inserting(" ") => r
    | Inserting(_) => l
    | Deleting(L) => l
    | Deleting(R) => r
    | Navigating => l
    };
  };
  // let m = pad_r ? Meld.mk(~l=c, w) : Meld.mk(w, ~r=c);
  // let m = Meld.mk(~l=c, w);
  Cell.put(m);
};

let rec repad = (~l=Delim.root, ~r=Delim.root, c: Cell.t) => {
  switch (Cell.Space.get(c)) {
  | None =>
    let m = Option.get(Cell.get(c));
    Meld.to_chain(m)
    |> Chain.map_link(Bound.node)
    |> Chain.consnoc(~hd=l, ~ft=r)
    |> Chain.map_linked((l, c, r) =>
         Cell.is_clean(c) ? c : repad(~l, c, ~r)
       )
    |> Chain.unconsnoc_exn
    |> (((_, c, _)) => c)
    |> Chain.map_link(Bound.get_exn)
    |> Meld.of_chain
    |> Cell.put;
  | Some(_) when Cell.has_clean_cursor(c) => Cell.mark_clean(c)
  | Some(_) =>
    let (_, pad_l) = Delim.padding(l).h;
    let (pad_r, _) = Delim.padding(r).h;
    switch (Cell.get(c)) {
    | None when !pad_l && !pad_r => c
    | None => pad_wrap(c)
    | Some(m) =>
      let pruned =
        Meld.to_chain(m)
        |> Chain.fold_right(
             (c, tok: Token.t, acc) => {
               let found_space = Result.is_ok(Chain.unlink(acc));
               switch (tok.mtrl) {
               | Space(White(Sys)) when found_space || !pad_l && !pad_r =>
                 Chain.map_hd(Cell.Space.merge(c, ~fill=Cell.empty), acc)
               | _ => Chain.link(c, tok, acc)
               };
             },
             Chain.unit,
           );
      switch (Chain.unlink(pruned)) {
      | Ok(_) => Cell.put(Meld.of_chain(pruned))
      | Error(c) when !pad_l && !pad_r => c
      | Error(c) => pad_wrap(c)
      };
    };
  };
};
