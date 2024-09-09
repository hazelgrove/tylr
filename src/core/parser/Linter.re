let pad_wrap = (~break=false, c: Cell.t) => {
  let text = break ? "\n" : " ";
  let w = Wald.of_tok(Token.mk(~text, Mtrl.Space(White(Sys))));
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
  // let m = spc_r ? Meld.mk(~l=c, w) : Meld.mk(w, ~r=c);
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
    let (_, spc_l) = Delim.padding(l).space;
    let (_, brk_l) = Delim.padding(l).break;
    let (spc_r, _) = Delim.padding(r).space;
    let (brk_r, _) = Delim.padding(r).break;
    let no_pad = !(spc_l || spc_r || brk_l || brk_r);
    let break = brk_l || brk_r;
    switch (Cell.get(c)) {
    | None when no_pad => c
    | None => pad_wrap(~break, c)
    | Some(m) =>
      let pruned =
        Meld.to_chain(m)
        |> Chain.fold_right(
             (c, tok: Token.t, acc) => {
               let found_space = Result.is_ok(Chain.unlink(acc));
               switch (tok.mtrl) {
               | Space(White(Sys)) when found_space || no_pad =>
                 Chain.map_hd(Cell.Space.merge(c, ~fill=Cell.empty), acc)
               | _ => Chain.link(c, tok, acc)
               };
             },
             Chain.unit,
           );
      switch (Chain.unlink(pruned)) {
      | Ok(_) => Cell.put(Meld.of_chain(pruned))
      | Error(c) when no_pad => c
      | Error(c) => pad_wrap(~break, c)
      };
    };
  };
};
