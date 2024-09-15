open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;

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
  let wrap = meld => mk(~meld, ());
  let empty = mk();
  let is_empty = (~require_unmarked=false, c: t(_)) =>
    Option.is_none(c.meld) && (!require_unmarked || Marks.is_empty(c.marks));

  let map_marks = (f, cell: t(_)) => {...cell, marks: f(cell.marks)};
  let add_marks = marks => map_marks(Marks.union(marks));
  let clear_marks = (cell: t(_)) => {...cell, marks: Marks.empty};
  let pop_marks = (cell: t(_)) => (cell.marks, clear_marks(cell));
  let put_cursor = (cur: Path.Cursor.t, cell: t(_)) => {
    ...cell,
    marks: Marks.put_cursor(cur, cell.marks),
  };

  let rec end_path = (~sans_padding=false, ~side: Dir.t, c: t(_)) =>
    switch (c.meld) {
    | None => Path.empty
    | Some(m) when sans_padding && Meld.is_space(m) =>
      end_path(~side=Dir.toggle(side), c)
    | Some(M(l, _, r) as m) =>
      let hd = Dir.pick(side, (0, Meld.length(m) - 1));
      let tl = end_path(~sans_padding, ~side, Dir.pick(side, (l, r)));
      Path.cons(hd, tl);
    };

  let is_clean = (c: t(_)) => Path.Map.is_empty(c.marks.dirty);
  let mark_clean = (c: t(_)) => {...c, marks: Marks.mark_clean(c.marks)};
  let mark_ends_dirty = (c: t(_)) => {
    let (l, r) = (end_path(~side=L, c), end_path(~side=R, c));
    let dirty = c.marks.dirty |> Path.Map.add(l, ()) |> Path.Map.add(r, ());
    let marks = {...c.marks, dirty};
    {...c, marks};
  };
  let has_clean_cursor = (c: t(_)) =>
    switch (c.marks.cursor) {
    | None => false
    | Some(Point(car)) => !Path.Map.mem(car.path, c.marks.dirty)
    | Some(Select(sel)) =>
      let (l, r) = sel.range;
      Path.Map.(!mem(l, c.marks.dirty) && !mem(r, c.marks.dirty));
    };

  // todo: move these out of Base
  let lift_tok_marks = (tok: Token.t): (Marks.t, Token.t) =>
    Token.pop_marks(tok)
    |> Tuples.map_fst(Marks.of_token)
    |> Tuples.map_fst(
         Option.is_some(Oblig.of_token(tok))
           ? Marks.add_oblig(tok.mtrl) : Fun.id,
       );
  let lower_tok_marks = (marks: Marks.t, tok: Token.t): Token.t =>
    Token.put_marks(Marks.to_token(~len=Token.length(tok), marks), tok);

  let aggregate_marks = (c: t(_)) =>
    switch (c.meld) {
    | None => c
    | Some(m) =>
      let (ics, its) =
        Meld.to_chain(m)
        |> Chain.mapi((i, c) => (i, c), (i, t) => (i, t));
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

  let distribute_marks = (c: t(_)) =>
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

  let get = (~distribute=true, c: t(_)) =>
    (distribute ? distribute_marks : Fun.id)(c).meld;
  let put = (meld: Meld.t(_)) => aggregate_marks(mk(~meld, ()));
};
include Base;

[@deriving (sexp, yojson)]
type t = Base.t(Token.t);

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

// module Wald = Meld.Wald;

// include Meld.Cell;
// [@deriving (show({with_path: false}), sexp, yojson)]
// type t = Meld.Cell.t(Meld.t);
// let empty = mk();
let face = (~side: Dir.t, c: t) => Option.map(Meld.face(~side), c.meld);

// let flatten = (cell: t) =>
//   cell.meld |> Option.map(Meld.flatten) |> Option.to_list |> List.flatten;

let rec mark_degrouted = (~side: Dir.t, c: t) =>
  switch (c.meld) {
  | None =>
    let marks = {...c.marks, degrouted: true};
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

  let unmark_degrouted = (c: t) => {
    let marks = {...c.marks, degrouted: false};
    {...c, marks};
  };

  let mk = (cs: list(t), ts: list(Token.t)) =>
    switch (cs, ts) {
    | ([c], []) => c
    | _ => put(Meld.of_chain((cs, ts)))
    };

  let split = (c: t) => {
    open Options.Syntax;
    assert(is_space(c));
    let* m = g(c);
    let (cs, ts) = Meld.to_chain(m);
    let (cs_l, cs_r) =
      cs |> Lists.split_while(~f=(c: t) => !c.marks.degrouted);
    switch (cs_l, cs_r) {
    | ([], _) =>
      let cs = List.map(unmark_degrouted, cs);
      Some((empty, put(Meld.of_chain((cs, ts)))));
    | (_, []) => None
    | ([_, ..._], [_, ..._]) =>
      let (ts_l, ts_r) = Lists.split_n(ts, List.length(cs_l));
      let cs_l = List.map(unmark_degrouted, cs_l) @ [empty];
      let cs_r = List.map(unmark_degrouted, cs_r);
      Some((mk(cs_l, ts_l), mk(cs_r, ts_r)));
    };
  };

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
          // Marks.map_paths(
          //   Lists.map_hd((+)(2 * List.length(Meld.tokens(m)))),
          // )
          let shift = 2 * List.length(Meld.tokens(m));
          Marks.map_paths(
            fun
            | [] => {
                assert(r.meld == None);
                [shift];
              }
            | [hd, ...tl] => [hd + shift, ...tl],
          );
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
    Base.{marks, meld};
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

let to_chain = (c: t) =>
  get(c)
  |> Option.map(Meld.to_chain)
  |> Option.value(~default=Chain.unit(c));
