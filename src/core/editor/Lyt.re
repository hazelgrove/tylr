module Base = {
  type t('tok) = Chain.t(line('tok), int)
  and line('tok) =
    | Line(list(chunk('tok)))
    | Block(t('tok))
  and chunk('tok) =
    | Token('tok)
    | Match(t('tok));

  let token = (tok: 'tok) => Chain.unit([Token(tok)]);
  let chunk = (b: t(_)) => Chain.unit([Block(b)]);

  let nil = Chain.unit([]);

  let hcat = (l: t(_), r: t(_)) =>
    Chain.cat((@), l, r);
  let vcat = (l: t(_), ~indent=0, r: t(_)) =>
    Chain.append(l, indent, r);

  let hcats = bs => List.fold_right(hcat, bs, nil);
  let vcats = bs => List.fold_right(vcat, bs, nil);

  let nest = (n: int, b: t(_)) =>
    Chain.map_links((+)(n), b);

  let height = ((_, newlines): t(_)) => List.length(newlines);

};
include Base;

module Dims = {
  type t = Base.t(int);



  let of_tok = (tok: Token.t) =>
    switch (tok.mtrl) {
    | Space () =>
      let lines =
        Strings.split('\n', tok.text)
        |> List.map(s => token(Utf8.length(s)));
      switch (Lists.Framed.ft(lines)) {
      | None => (vcats(lines), nil)
      | Some((lines, last)) => (vcats(List.rev(lines)), last)
      };
    | Grout(_) => (token(1), nil)
    | Tile(_) => (token(Token.length(tok)), nil)
    };

  let rec of_cell = (~delim=Delim.root, c: Cell.t): (t, t) =>
    switch (Cell.get(c)) {
    | None => (nil, nil)
    | Some(m) => of_meld(m)
    }
  and of_meld = (M(l, w, r) as m: Meld.t): (t, t) =>
    switch (Meld.Space.get(m)) {
    | Some(spc) => of_space(spc)
    | None =>
      let l = Funs.uncurry(hcat, of_cell(l));
      let w = of_wald(w);
      let (r, r_ft) = of_cell(~delim=Node(Wald.ft(w)), r);
      let indent = Delim.indent(delim) && height(l) > 0 ? 2 : 0;
      (nest(indent, hcats([l, w, r])), r_ft);
    }
  and of_wald = (W(toks, cells): Wald.t): t =>
}

// module Base = {
//   type t('text) =
//     | Text('text)
//     // Hbox([x]) == x
//     | Hbox(list(t('text)))
//     // Vbox(unit(x)) == x
//     | Vbox(Chain.t(t('text), int))
//     | Chunk(t('text));
//   let nil = Hbox([]);
//   let rec height =
//     fun
//     | Text(_)
//     | Chunk(_) => 0
//     | Hbox(ls) =>
//       ls
//       |> List.fold_left(l => (+)(height(l)), 0)
//     | Vbox(((ls, is))) =>
//       ls
//       |> List.fold_left(l => (+)(height(l)), 0)
//       |> (+)(List.length(is));
//   let rec normalize =
//     fun
//     | Text(t)
//     |
// };
// include Base;

module Dims = {
  type t = Base.t(int);

  let rec of_cell = (~delim=Delim.root, c: Cell.t) =>
    switch (Cell.get(c)) {
    | None => (nil, nil, nil)
    | Some(M(l, w, r)) =>
      let (p_ll, l, p_lr) = of_cell(~delim, l);
      let w = Chunk(of_wald(w));
      let (p_rl, r, p_rr) = of_cell(~delim=Node(Wald.ft(w)), r);y
      (p_ll, )





      let ((l, h_l), w, (r, h_r)) => of_meld(m);
      Seq([
        Nest(indent, l),
        Tag
        Nest(indent, r),
      ])
    }
  and of_meld = (M(l, w, r): Meld.t) => (
    of_cell(l),
    of_wald(w),
    of_cell(~delim=Node(Wald.ft(w)), r),
  )
  and of_wald = (w: Wald.t) => failwith("todo");
};

