open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;

let dbg = ref(false);

module Wald = {
  [@deriving (sexp, yojson)]
  type t('tok, 'cell) =
    | W(Chain.t('tok, 'cell));
  let pp = (pp_tok, pp_cell, out, W(w): t(_)) =>
    Chain.pp(pp_tok, pp_cell, out, w);
  let show = (pp_tok, pp_cell) => Fmt.to_to_string(pp(pp_tok, pp_cell));
  let face = (~side=Dir.L, W(w): t(_)) =>
    Dir.pick(side, (Chain.hd, Chain.ft), w);
  let of_tok = tok => W(Chain.unit(tok));
  let append = (W(l): t(_), m, W(r): t(_)) => W(Chain.append(l, m, r));
};
module Meld = {
  [@deriving (sexp, yojson)]
  type t('cell, 'tok) =
    | M('cell, Wald.t('tok, 'cell), 'cell);

  let mk = (~l, w, ~r) => M(l, w, r);
  let face = (~side: Dir.t, M(_, w, _)) => Wald.face(~side, w);
  let length = (M(_, W(w), _): t(_)) => Chain.length(w) + 2;
  let tokens = (M(_, W((toks, _)), _): t(_)) => toks;

  let rev = (M(l, W(w), r): t(_)) => mk(~l=r, W(Chain.rev(w)), ~r=l);

  let to_chain = (M(l, W((ts, cs)), r): t(_)) => ([l, ...cs] @ [r], ts);
  let of_chain = ((cs, ts): Chain.t('cell, 'tok)) => {
    let get = Options.get_exn(Invalid_argument("Meld.of_chain"));
    // cs reversed twice
    let (cs, r) = get(Lists.Framed.ft(cs));
    let (cs, l) = get(Lists.Framed.ft(cs));
    M(l, W((ts, cs)), r);
  };

  let pp = (pp_cell, pp_tok, out, m: t(_)) =>
    Chain.pp(pp_cell, pp_tok, out, to_chain(m));
  let show = (pp_cell, pp_tok) => Fmt.to_to_string(pp(pp_cell, pp_tok));

  let get_space = (M(_, W((toks, _)), _): t(_, Token.t)) =>
    List.for_all(Token.Space.is, toks) ? Some(toks) : None;
  let is_space = m => Option.is_some(get_space(m));
};

module Marks = Marks.Cell;
module Base = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('tok) = {
    marks: Marks.t,
    meld: option(Meld.t(t('tok), 'tok)),
  };
  let mk = (~marks=Marks.empty, ~meld=?, ()) => {marks, meld};
  let empty = mk();
  let wrap = meld => mk(~meld, ());
};
include Base;

[@deriving (sexp, yojson)]
type t = Base.t(Token.t);

let rec flatten: Base.t(Token.t) => list('a) =
  c =>
    switch (c.meld) {
    | None => []
    | Some(m) => flatten_meld(m)
    }
and flatten_wald = (W(wald): Wald.t(_)) =>
  Chain.map(Fun.id, flatten, wald)
  |> Chain.to_list(x => [x], Fun.id)
  |> List.flatten
and flatten_meld = (M(p_left, wald, p_right): Meld.t(_)) =>
  flatten(p_left) @ flatten_wald(wald) @ flatten(p_right);

let dirty = mk(~marks=Marks.dirty, ());
let rec pp = (out, {marks, meld}: t) => {
  let pp_meld = Meld.pp(pp, Token.pp);
  if (Marks.is_empty(marks) && Option.is_none(meld)) {
    Fmt.pf(out, "{}");
  } else if (Marks.is_empty(marks)) {
    Fmt.pf(out, "{@[<hov 2>%a@]}", Fmt.option(pp_meld), meld);
  } else {
    Fmt.pf(
      out,
      "{@[<hov 2>@[%a@] |@ @[%a@]@]}",
      Marks.pp,
      marks,
      Fmt.option(pp_meld),
      meld,
    );
  };
};
let show = Fmt.to_to_string(pp);

let is_empty = (~require_unmarked=false, c: t) =>
  Option.is_none(c.meld) && (!require_unmarked || Marks.is_empty(c.marks));

let put_cursor = (cur: Path.Cursor.t, cell: t) => {
  ...cell,
  marks: Marks.put_cursor(cur, cell.marks),
};

let face = (~side: Dir.t, c: t) => Option.map(Meld.face(~side), c.meld);

let rec height = (~side: Dir.t, c: t) =>
  switch (c.meld) {
  | None => 0
  | Some(M(l, _, r)) => 1 + height(~side, Dir.pick(side, (l, r)))
  };

// let flatten = (cell: t) =>
//   cell.meld |> Option.map(Meld.flatten) |> Option.to_list |> List.flatten;

let map_marks = (f, cell: t) => {...cell, marks: f(cell.marks)};
let add_marks = marks => map_marks(Marks.union(marks));
let clear_marks = (cell: t) => {...cell, marks: Marks.empty};
let pop_marks = (cell: t) => (cell.marks, clear_marks(cell));

let rec mark_degrouted = (~side: Dir.t, c: t) =>
  switch (c.meld) {
  | None =>
    let marks = {...c.marks, degrouted: Path.Map.singleton(Path.empty, ())};
    {...c, marks};
  | Some(M(l, w, r)) =>
    switch (side) {
    | L =>
      let l = mark_degrouted(~side, l);
      {...c, meld: Some(M(l, w, r))};
    | R =>
      let r = mark_degrouted(~side, r);
      {...c, meld: Some(M(l, w, r))};
    }
  };
let unmark_degrouted = (c: t) => {
  let marks = {...c.marks, degrouted: Path.Map.empty};
  {...c, marks};
};

let rec end_path = (~sans_padding=false, ~side: Dir.t, c: t) =>
  switch (c.meld) {
  | None => Path.empty
  | Some(m) when sans_padding && Meld.is_space(m) =>
    end_path(~side=Dir.toggle(side), c)
  | Some(M(l, _, r) as m) =>
    let hd = Dir.pick(side, (0, Meld.length(m) - 1));
    let tl = end_path(~sans_padding, ~side, Dir.pick(side, (l, r)));
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

let get = (~distribute=true, c: t) =>
  (distribute ? distribute_marks : Fun.id)(c).meld;
let put = (meld: Meld.t(_)) => aggregate_marks(mk(~meld, ()));

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
           (spc, c: t, acc) => {
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

  let g = get;
  let get = (c: t) =>
    switch (get(c)) {
    | None => Some([])
    | Some(m) => Meld.get_space(m)
    };
  let is_space = c => Option.is_some(get(c));

  let mk = (cs: list(t), ts: list(Token.t)) =>
    switch (cs, ts) {
    | ([c], []) => c
    | _ => put(Meld.of_chain((cs, ts)))
    };

  // returns split-off cell first (regardless of side), rest of cell second
  let split = (~side as d: Dir.t, c: t) => {
    open Options.Syntax;
    assert(is_space(c));
    let* m = g(c);
    let (cs, ts) = m |> Dir.pick(d, (Fun.id, Meld.rev)) |> Meld.to_chain;
    let (cs_d, cs_b) =
      cs
      |> Lists.split_while(~f=(c: t) => Path.Map.is_empty(c.marks.degrouted));
    switch (cs_d, cs_b) {
    | (_, []) => None
    | ([], [b_hd, ...b_tl]) =>
      let b_hd_dup = {
        ...b_hd,
        marks: {
          ...b_hd.marks,
          cursor: None,
        },
      };
      let rest =
        Meld.of_chain(([b_hd_dup, ...b_tl], ts))
        |> Dir.pick(d, (Fun.id, Meld.rev))
        |> put;
      Some((b_hd, rest));
    | ([_, ..._], [b_hd, ...b_tl]) =>
      let (ts_d, ts_b) = Lists.split_n(ts, List.length(cs_d));
      let cs_d = cs_d @ [b_hd];
      let b_hd_dup = {
        ...b_hd,
        marks: {
          ...b_hd.marks,
          cursor: None,
        },
      };
      let cs_b = [b_hd_dup, ...b_tl];
      let split =
        (cs_d, ts_d)
        |> Dir.pick(d, (Fun.id, c => Chain.rev(c)))
        |> Funs.uncurry(mk);
      let rest =
        (cs_b, ts_b)
        |> Dir.pick(d, (Fun.id, c => Chain.rev(c)))
        |> Funs.uncurry(mk);
      Some((split, rest));
    };
  };

  let merge = (l: t, ~fill=empty, r: t) => {
    if (!is_space(l) || Option.is_some(fill.meld) || !is_space(r)) {
      raise(Invalid_argument("Cell.Space.merge"));
    };
    switch (g(l), g(r)) {
    | (None, None) =>
      let marks = Marks.union_all([l.marks, fill.marks, r.marks]);
      Base.mk(~marks, ());
    | (None, Some(_)) =>
      let marks =
        Marks.(union_all([cons(0, l.marks), cons(0, fill.marks), r.marks]));
      {...r, marks};
    | (Some(m_l), None) =>
      let shift = 2 * List.length(Meld.tokens(m_l));
      let marks =
        Marks.(
          union_all([
            l.marks,
            cons(shift, fill.marks),
            cons(shift, r.marks),
          ])
        );
      {...l, marks};
    | (Some(M(c_l, w_l, c_m_l)), Some(M(c_m_r, w_r, c_r))) =>
      assert(c_m_l.meld == None && c_m_r.meld == None);
      let c_m = {
        ...fill,
        marks: Marks.union_all([c_m_l.marks, fill.marks, c_m_r.marks]),
      };
      put(M(c_l, Wald.append(w_l, c_m, w_r), c_r));
    };
  };
};

let get_spc = (c: t) =>
  switch (get(c)) {
  | None => Some([])
  | Some(m) => Meld.get_space(m)
  };

let rec pad = (~squash=false, ~l=empty, ~r=empty, c: t) =>
  switch (get(c)) {
  | _ when l == empty && r == empty => c
  | None => Space.merge(l, ~fill=c, r) |> (squash ? Space.squash : Fun.id)
  | Some(m) when Meld.is_space(m) =>
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
