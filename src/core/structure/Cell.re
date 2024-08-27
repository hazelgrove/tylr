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

let rec end_path = (~side: Dir.t, c: t) =>
  switch (c.meld) {
  | None => Path.empty
  | Some(M(l, _, r) as m) =>
    let hd = Dir.pick(side, (0, Meld.length(m) - 1));
    let tl = end_path(~side, Dir.pick(side, (l, r)));
    Path.cons(hd, tl);
  };

let lift_tok_marks = (tok: Token.t): (Marks.t, Token.t) =>
  Token.pop_marks(tok)
  |> Tuples.map_fst(Marks.of_token)
  |> Tuples.map_fst(
       Option.is_some(Oblig.of_token(tok))
         ? Marks.add_oblig(tok.mtrl) : Fun.id,
     );
let lower_tok_marks = (marks: Marks.t, tok: Token.t): Token.t =>
  Token.put_marks(Marks.to_token(~len=Token.length(tok), marks), tok);

let aggregate_marks = (c: t) =>
  switch (c.meld) {
  | None => c
  | Some(m) =>
    let (ics, its) =
      Meld.to_chain(m) |> Chain.mapi((i, c) => (i, c), (i, t) => (i, t));
    let (cs_marks, cs) =
      ics
      |> List.map(((i, c)) =>
           Tuples.map_fst(Marks.cons(i), pop_marks(c))
         )
      |> List.split;
    let (ts_marks, ts) =
      Chain.linked_loops((ics, its))
      |> List.map((((_, l), (i, t), (_, r))) =>
           lift_tok_marks(t)
           |> Tuples.map_fst(
                Marks.map_paths(
                  fun
                  // normalize cursors at ends of tokens to ends of cells
                  | [n] when n <= 0 => [i - 1, ...end_path(l, ~side=R)]
                  | [n] when n >= Token.length(t) => [
                      i + 1,
                      ...end_path(~side=L, r),
                    ]
                  | p => [i, ...p],
                ),
              )
         )
      |> List.split;
    {
      marks: Marks.union_all(cs_marks @ ts_marks),
      meld: Some(Meld.of_chain((cs, ts))),
    };
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

let caret = (car: Path.Caret.t) =>
  mk(~marks=Marks.mk(~cursor=Point(car), ()), ());

// todo: clean up point/caret terminology
let point = (~dirty=false, hand: Caret.Hand.t) => {
  let dirty = Path.Map.(dirty ? singleton(Path.empty, ()) : empty);
  let cursor = Cursor.Point(Caret.mk(hand, Path.empty));
  mk(~marks=Marks.mk(~cursor, ~dirty, ()), ());
};
let is_caret = (c: t): option(Caret.Hand.t) =>
  if (c == point(Focus)) {
    Some(Focus);
  } else if (c == point(Anchor)) {
    Some(Anchor);
  } else {
    None;
  };

module Space = {
  let squash = (c: t) =>
    switch (get(c)) {
    | Some(M(l, W((toks, _) as w), r))
        when List.for_all(Token.Space.is, toks) =>
      w
      |> Chain.fold_right(
           (spc, c, acc) => {
             let caret =
               c.marks.cursor
               |> Options.bind(~f=Cursor.get_point)
               |> Option.map(Caret.hand);
             Token.Space.squash(spc, ~caret?, acc);
           },
           Fun.id,
         )
      |> Wald.of_tok
      |> Meld.mk(~l, ~r)
      |> put
    | _ => c
    };

  let get = (c: t) =>
    switch (get(c)) {
    | None => Some([])
    | Some(m) => Meld.Space.get(m)
    };
  let is_space = c => Option.is_some(get(c));

  let merge = (l: t, ~fill=empty, r: t) => {
    if (!is_space(l) || !is_space(r)) {
      raise(Invalid_argument("Cell.Space.merge"));
    };
    let marks =
      r.marks
      |> (
        switch (l.meld) {
        | None => Fun.id
        | Some(m) =>
          Marks.map_paths(
            Lists.map_hd((+)(2 * List.length(Meld.tokens(m)))),
          )
        }
      )
      |> Marks.union(l.marks);
    let meld =
      Options.merge(
        l.meld,
        r.meld,
        ~f=(Meld.M(l, w_l, m_l), Meld.M(m_r, w_r, r)) => {
          let m = {
            ...fill,
            marks: Marks.union_all([m_l.marks, fill.marks, m_r.marks]),
          };
          Meld.M(l, Wald.append(w_l, m, w_r), r);
        },
      );
    {marks, meld};
  };
};

let get_spc = (c: t) =>
  switch (get(c)) {
  | None => Some([])
  | Some(m) => Meld.Space.get(m)
  };

let rec pad = (~squash=false, ~l=empty, ~r=empty, c: t) =>
  switch (get(c)) {
  | _ when l == empty && r == empty => c
  | None => Space.merge(l, ~fill=c, r) |> (squash ? Space.squash : Fun.id)
  | Some(m) when Option.is_some(Meld.Space.get(m)) =>
    Space.merge(l, Space.merge(c, r)) |> (squash ? Space.squash : Fun.id)
  | Some(M(c_l, w, c_r)) =>
    let c_l = pad(~l, c_l);
    let c_r = pad(c_r, ~r);
    put(M(c_l, w, c_r));
  };

let prune_sys = (c: t) =>
  switch (get(c)) {
  | None => c
  | Some(m) =>
    let pruned =
      Meld.to_chain(m)
      |> Chain.fold_right(
           (c, tok: Token.t) =>
             switch (tok.mtrl) {
             | Space(White(Sys)) =>
               // drop tok
               Chain.map_hd(Space.merge(c, ~fill=empty))
             | _ => Chain.link(c, tok)
             },
           Chain.unit,
         );
    switch (Chain.unlink(pruned)) {
    | Error(c) => c
    | Ok(_) => put(Meld.of_chain(pruned))
    };
  };

let is_clean = (c: t) => Path.Map.is_empty(c.marks.dirty);
let mark_clean = (c: t) => {...c, marks: Marks.mark_clean(c.marks)};
let mark_ends_dirty = (c: t) => {
  let (l, r) = (end_path(~side=L, c), end_path(~side=R, c));
  let dirty = c.marks.dirty |> Path.Map.add(l, ()) |> Path.Map.add(r, ());
  let marks = {...c.marks, dirty};
  {...c, marks};
};

let has_clean_cursor = (c: t) =>
  switch (c.marks.cursor) {
  | None => false
  | Some(Point(car)) => !Path.Map.mem(car.path, c.marks.dirty)
  | Some(Select(sel)) =>
    let (l, r) = sel.range;
    Path.Map.(!mem(l, c.marks.dirty) && !mem(r, c.marks.dirty));
  };

let to_chain = (c: t) =>
  get(c)
  |> Option.map(Meld.to_chain)
  |> Option.value(~default=Chain.unit(c));
