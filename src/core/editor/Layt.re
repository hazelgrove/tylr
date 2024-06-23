// module type LINE = {
//   type t;
//   let nil: t;
//   let cat: (t, t) => t;
// };

module Line = {
  type t = list(Token.t);
  let nil = [];
  let cat = (@);
};
module Block = {
  type t = Chain.t(section, int)
  and section =
    | Line(Line.t)
    | Block(t);

  let sec = Chain.unit;
  let line = l => sec(Line(l));

  let nil = line(Line.nil);

  let wrap = (b: t) => sec(Block(b));

  let cons = (sec: section, ~indent=0) => Chain.link(sec, indent);

  let rec cat_hd = (line: Line.t, block: t) =>
    block
    |> Chain.map_hd(
         fun
         | Line(l) => Line.cat(line, l)
         | Block(b) => cat_hd(line, b),
       );
  let rec cat_ft = (block: t, line: Line.t) =>
    block
    |> Chain.map_ft(
         fun
         | Line(l) => Line.cat(l, line)
         | Block(b) => cat_ft(line, b),
       );

  let rec hcat = (l: t, r: t) => Chain.cat(hcat_sec, l, r)
  and hcat_sec = (l, r) =>
    switch (l, r) {
    | (Line(l), Line(r)) => Line(Line.cat(l, r))
    | (Line(l), Block(r)) => Block(cat_hd(l, r))
    | (Block(l), Line(r)) => Block(cat_ft(l, r))
    | (Block(l), Block(r)) => Block(hcat(l, r))
    };

  let vcat = (l: t, ~indent=0, r: t) => Chain.append(l, indent, r);

  // blocks carry indentation info for all but the first section
  let nest_tl = (n: int) => Chain.map_link((+)(n));
  let nest_body = (n: int, b: t) =>
    switch (Chain.unconsnoc(b)) {
    | _ when n == 0 => b
    // one section
    | Error(sec) => b
    | Ok((l, m, r)) =>
      switch (Chain.unconsnoc(m)) {
      // two sections
      | Error(_) when Section.is_space(r) => b
      | Error(ind) => cons(l, ~indent=ind + n, sec(r))
      // more sections
      | Ok((ind_l, m, ind_r)) =>
        let ind_l = ind_l + n;
        let ind_r = ind_r + (Section.is_space(r) ? 0 : n);
        nest_tl(n, m)
        |> Chain.consnoc(~hd=ind_l, ~ft=ind_r)
        |> Chain.consnoc(~hd=l, ~ft=r);
      }
    };
};
include Block;

let of_tok = (tok: Token.t) =>
  switch (tok.mtrl) {
  | Space =>
    Strings.split('\n', tok.text)
    |> List.map(text => {...tok, text})
    |> Block.line
  | Grout(_)
  | Tile(_) => Block.line([tok])
  };

let of_cell = (~delim=Delim.root, c: Cell.t): t =>
  switch (Cell.get(c)) {
  | None => nil
  | Some(m) =>
    let (l, w, r) = of_meld(m);
    let indent = Delim.indent(delim) && height(l) > 0 ? 2 : 0;
    nest_body(indent, hcats([l, w, r]));
  }
and of_meld = (m: Meld.t): (t, t, t) => {
  let (l, w, r) =
    Meld.to_chain(m)
    |> Chain.fold_left(
         l => Chain.unit(of_cell(l)),
         (acc, tok, cell) =>
           Chain.link(of_cell(~delim=Node(tok)), of_tok(tok), acc),
       )
    |> Chain.rev
    |> Chain.unconsnoc
    |> Result.unwrap;
  let w =
    w
    |> Chain.fold_right(
         (b_tok, b_cell, b_acc) => hcats([b_tok, b_cell, b_acc]),
         Fun.id,
       )
    |> Block.wrap;
  (l, w, r);
};

module State = {
  type t = {
    ind: Col.t,
    loc: Loc.t,
  };

  let init = {
    ind: 0,
    loc: Loc.zero,
  };

  let map = (f, s: t) => {...s, loc: f(loc)};

  let indent = (n: int, s: t) => {ind: s.ind + n, loc: Loc.shift(n, s.loc)};
  let return = (s: t, ~ind: Col.t) => {ind, loc: Loc.return(s.loc, ~ind)};

  let rec jump_block = (s: t, ~over: Block.t) => {
    let ind = s.ind;
    over
    |> Chain.fold(
      sec => jump_sec(s, ~over=sec),
      (s, n, sec) => return(s, ~ind=ind + n) |> jump_sec(~over=sec),
    )
  }
  and jump_sec = (s: t, ~over: Block.section) =>
    switch (over) {
    | Line(l) => map(Loc.shift(Line.len(l)), s)
    | Block(b) => jump_block(~over=b)
    };

  let load_cell_frame = ((pre, suf), state: t) =>
    switch (pre, suf) {
    | ()
    }
};

let rec cursor = (~state=State.init, cell: Cell.t): option(Loc.Cursor.t) =>