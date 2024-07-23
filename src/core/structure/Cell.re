open Stds;

module Marks = Marks.Cell;
module Wald = Meld.Wald;

include Meld.Cell;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Meld.Cell.t(Meld.t);
// let empty = mk();
let is_empty = (~require_unmarked=false, c) =>
  Option.is_none(c.meld) && (!require_unmarked || Marks.is_empty(c.marks));

let put_cursor = (cur: Path.Cursor.t, cell: t) => {
  ...cell,
  marks: Marks.put_cursor(cur, cell.marks),
};

let face = (~side: Dir.t, c: t) => Option.map(Meld.face(~side), c.meld);

let flatten = (cell: t) =>
  cell.meld |> Option.map(Meld.flatten) |> Option.to_list |> List.flatten;

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

let get = (~distribute=true, c) =>
  (distribute ? distribute_marks : Fun.id)(c).meld;
let put = (meld: Meld.t) => aggregate_marks(mk(~meld, ()));

let get_spc = (c: t) =>
  switch (get(c)) {
  | None => Some(Token.empty())
  | Some(m) => Meld.Space.get(m)
  };
// raises Invalid_argument if inputs are not space cells
let merge_spc = (l: t, r: t) => {
  let get = Options.get_exn(Invalid_argument("Cell.merge"));
  let spc_l = get(get_spc(l));
  let spc_r = get(get_spc(r));
  let marks_l =
    l.marks
    |> Marks.map_paths(
         fun
         | [2, ..._] when !Token.is_empty(spc_r) => [
             1,
             Token.length(spc_l),
           ]
         | path => path,
       );
  let marks_r =
    r.marks
    |> Marks.map_paths(
         fun
         | []
         | [0, ..._] => [1, Token.length(spc_l)]
         | [1] => [1, Token.length(spc_l)]
         | [1, j, ..._] => [1, Token.length(spc_l) + j]
         | path => path,
       );
  let spc = Token.cat(spc_l, spc_r);
  let meld = Token.is_empty(spc) ? None : Some(Meld.of_tok(spc));
  let marks = Marks.union(marks_l, marks_r);
  {meld, marks};
};

let rec pad = (~l=empty, ~r=empty, c: t) =>
  switch (get(c)) {
  | _ when l == empty && r == empty => c
  | None => merge_spc(l, merge_spc(c, r))
  | Some(m) when Option.is_some(Meld.Space.get(m)) =>
    merge_spc(l, merge_spc(c, r))
  | Some(M(c_l, w, c_r)) =>
    let c_l = pad(~l, c_l);
    let c_r = pad(c_r, ~r);
    put(M(c_l, w, c_r));
  };

// let rec pad = (~l=?, ~r=?, c: t) => {
//   switch (get(c)) {
//   | _ when Option.(is_none(l) && is_none(r)) => c
//   | None =>
//     let caret =
//       Option.bind(
//         c.marks.cursor,
//         fun
//         | Point(car) => Some(car.hand)
//         | Select(_) => Some(Focus),
//       );
//     let l = Option.value(l, ~default=Token.empty());
//     let r = Option.value(r, ~default=Token.empty());
//     let t = Token.cat(l, ~caret?, r);
//     let w = Wald.of_tok(t);
//     let m = Meld.mk(~l=c, w);
//     put(m);
//   | Some(m) when Option.is_some(Meld.Space.get(m)) => c
//   // when Token.Space.is(spc) && !Token.is_empty(spc) => c
//   | Some(M(c_l, w, c_r)) =>
//     let c_l = pad(~l?, c_l);
//     let c_r = pad(c_r, ~r?);
//     put(M(c_l, w, c_r));
//   };
// };

let repad = (~l=false, ~r=false, c: t) =>
  switch (get(c)) {
  | _ when !l && !r => c
  | None =>
    let w = Wald.of_tok(Token.mk(~text=" ", Mtrl.Space()));
    let m = Meld.mk(~l=c, w);
    put(m);
  | Some(M(_, W(([spc], [])), _))
      when Token.Space.is(spc) && !Token.is_empty(spc) => c
  | Some(M(_)) =>
    let l = l ? put(Meld.of_tok(Token.space())) : empty;
    let r = r ? put(Meld.of_tok(Token.space())) : empty;
    pad(~l, c, ~r);
  };

let rec end_path = (~side: Dir.t, c: t) =>
  switch (get(c)) {
  | None => Path.empty
  | Some(M(l, _, r) as m) =>
    let hd = Dir.pick(side, (0, Meld.length(m) - 1));
    let tl = end_path(~side, Dir.pick(side, (l, r)));
    Path.cons(hd, tl);
  };

let caret = (car: Path.Caret.t) =>
  mk(~marks=Marks.mk(~cursor=Point(car), ()), ());

let point = (hand: Caret.Hand.t) =>
  mk(~marks=Marks.(mk(~cursor=Point(Caret.mk(hand, Path.empty)), ())), ());
let is_caret = (c: t): option(Caret.Hand.t) =>
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
    let spc = Token.cat(l_spc, r_spc);
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
