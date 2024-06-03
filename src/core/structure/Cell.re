open Util;

module Marks = Marks.Cell;

include Meld.Cell;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Meld.Cell.t(Meld.t);
// let empty = mk();
let is_empty = c => Option.is_none(c.meld);

let put_cursor = (cur: Path.Cursor.t, cell: t) => {
  ...cell,
  marks: Marks.put_cursor(cur, cell.marks),
};

let face = (~side: Dir.t, c: t) => Option.map(Meld.face(~side), c.meld);

let map_marks = (f, cell) => {...cell, marks: f(cell.marks)};
let add_marks = marks => map_marks(Marks.union(marks));
let clear_marks = cell => {...cell, marks: Marks.empty};
let pop_marks = cell => (cell.marks, clear_marks(cell));

let lift_tok_marks = (tok: Token.t): (Marks.t, Token.t) =>
  Token.pop_marks(tok)
  |> Tuples.map_fst(Marks.of_token)
  |> Tuples.map_fst(
       switch (Token.Tile.is_ghost(tok)) {
       | Some(t) => Marks.add_ghost(t)
       | None => Fun.id
       },
     );
let lower_tok_marks = (marks: Marks.t, tok: Token.t): Token.t =>
  Token.put_marks(Marks.to_token(~len=Token.length(tok), marks), tok);

let aggregate_marks = (c: t) =>
  switch (c.meld) {
  | None => c
  | Some(M(l, W((ts, cs)), r) as m) =>
    let map_cons = n => Tuples.map_fst(Marks.cons(n));
    let (l_marks, l) = map_cons(0, pop_marks(l));
    let (ts_marks, ts) =
      ts
      |> List.mapi((i, t) => map_cons(1 + 2 * i, lift_tok_marks(t)))
      |> List.split;
    let (cs_marks, cs) =
      cs
      |> List.mapi((i, c) => map_cons(2 * (1 + i), pop_marks(c)))
      |> List.split;
    let (r_marks, r) = map_cons(Meld.length(m) - 1, pop_marks(r));
    let marks =
      Marks.union_all(
        List.concat([[l_marks], ts_marks, cs_marks, [r_marks]]),
      );
    {marks, meld: Some(M(l, W((ts, cs)), r))};
  };

let distribute_marks = (c: t) =>
  switch (c.meld) {
  | None => c
  | Some(M(l, W((ts, cs)), r) as m) =>
    let l = l |> add_marks(Marks.peel(0, c.marks));
    let ts =
      ts |> List.mapi(i => lower_tok_marks(Marks.peel(1 + 2 * i, c.marks)));
    let cs =
      cs |> List.mapi(i => add_marks(Marks.peel(2 * (1 + i), c.marks)));
    let r = r |> add_marks(Marks.peel(Meld.length(m) - 1, c.marks));
    mk(~meld=Meld.M(l, W((ts, cs)), r), ());
  };

let get = c => distribute_marks(c).meld;
let put = (meld: Meld.t) => aggregate_marks(mk(~meld, ()));

let rec end_path = (~side: Dir.t, c: t) =>
  switch (get(c)) {
  | None => Path.empty
  | Some(M(l, _, r) as m) =>
    let hd = Dir.pick(side, (0, Meld.length(m) - 1));
    let tl = end_path(~side, Dir.pick(side, (l, r)));
    Path.cons(hd, tl);
  };

let point = (hand: Caret.Hand.t) =>
  mk(~marks=Marks.(mk(~cursor=Point(Caret.mk(hand, Path.empty)), ())), ());
let is_point = (c: t): option(Caret.Hand.t) =>
  if (c == point(Focus)) {
    Some(Focus);
  } else if (c == point(Anchor)) {
    Some(Anchor);
  } else {
    None;
  };

module Space = {
  let get = (c: t) =>
    switch (get(c)) {
    | None => Some(Token.Space.empty)
    | Some(m) => Meld.Space.get(m)
    };
  let is_space = c => Option.is_some(get(c));

  let merge = (l: t, r: t) => {
    open Options.Syntax;
    let+ l_spc = get(l)
    and+ r_spc = get(r);
    let spc = Token.merge(l_spc, r_spc);
    if (Token.is_empty(spc)) {
      {meld: None, marks: Marks.union(l.marks, r.marks)};
    } else {
      {
        meld: Some(Meld.of_tok(spc)),
        marks:
          Marks.union(
            l.marks,
            r.marks
            |> Marks.map_paths(
                 fun
                 | []
                 | [0, ..._] => [1, Token.length(l_spc)]
                 | [1] => [1, Token.length(l_spc)]
                 | [1, j, ..._] => [1, Token.length(l_spc) + j]
                 | path => path,
               ),
          ),
      };
    };
  };
};
