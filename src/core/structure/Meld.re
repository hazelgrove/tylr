open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;

module Marks = Marks.Cell;

module Cell = {
  [@deriving (sexp, yojson)]
  type t('meld) = {
    marks: Marks.t,
    meld: option('meld),
  };
  let mk = (~marks=Marks.empty, ~meld=?, ()) => {marks, meld};
  let empty = mk();
  let pp = (pp_meld, out, {marks, meld}: t(_)) =>
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
  let show = pp_meld => Fmt.to_to_string(pp(pp_meld));
};

module Wald = {
  [@deriving (sexp, yojson)]
  type t('cell) =
    | W(Chain.t(Token.t, 'cell));
  let mk = (toks: list(_), cells: list(Cell.t(_))) =>
    W(Chain.mk(toks, cells));
  let of_tok = tok => W(Chain.unit(tok));
  let face = (~side=Dir.L, W(w): t(_)) =>
    Dir.pick(side, (Chain.hd, Chain.ft), w).mtrl;
  let pp = (pp_cell, out, W(w): t(_)) =>
    Chain.pp(Token.pp, pp_cell, out, w);
  let show = pp_cell => Fmt.to_to_string(pp(pp_cell));
  let append = (W(l): t(_), m, W(r): t(_)) => W(Chain.append(l, m, r));
};

module Base = {
  [@deriving (sexp, yojson)]
  type t =
    | M(Cell.t(t), Wald.t(Cell.t(t)), Cell.t(t));
  let mk = (~l=Cell.empty, ~r=Cell.empty, w) => M(l, w, r);
  let to_chain = (M(l, W((ts, cs)), r): t) => ([l, ...cs] @ [r], ts);
  let of_chain = ((cs, ts): Chain.t(Cell.t(_), Token.t)) => {
    let get = Options.get_exn(Invalid_argument("Meld.of_chain"));
    // cs reversed twice
    let (cs, r) = get(Lists.Framed.ft(cs));
    let (cs, l) = get(Lists.Framed.ft(cs));
    mk(~l, W((ts, cs)), ~r);
  };
  let rec pp = (out, m: t) =>
    Chain.pp(Cell.pp(pp), Token.pp, out, to_chain(m));
  let show = Fmt.to_to_string(pp);
};
include Base;

let of_tok = (~l=Cell.empty, ~r=Cell.empty, tok) =>
  mk(~l, Wald.of_tok(tok), ~r);

let is_empty =
  fun
  | M({meld: None, _}, W(([tok], [])), {meld: None, _}) =>
    Token.is_empty(tok)
  | _ => false;

let tokens = (M(_, W((toks, _)), _): t) => toks;

let fold = (f_hd, f_tl, m: t) => Chain.fold_left(f_hd, f_tl, to_chain(m));

let rec flatten = (m: t): list(Token.t) =>
  to_chain(m)
  |> Chain.to_list(
       (c: Cell.t(t)) =>
         c.meld |> Option.map(flatten) |> Option.to_list |> List.flatten,
       Lists.single,
     )
  |> List.flatten;

module Affix = {
  include Chain.Affix;
  type t = Chain.Affix.t(Token.t, Cell.t(Base.t));
};

module Space = {
  let mk = (tok: Token.t) => {
    assert(Mtrl.is_space(tok.mtrl));
    mk(Wald.of_tok(tok));
  };
  let get =
    fun
    | M(_, W((toks, _)), _) when List.for_all(Token.Space.is, toks) =>
      Some(toks)
    | _ => None;
};
module Grout = {
  let op_ = (s: Sort.t) => mk(Wald.of_tok(Token.Grout.op_(s)));
};

let split_subwald = (i, j, M(l, W((ts, cs)), r): t) => {
  let i = i / 2;
  let j = j / 2 - (j mod 2 == 0 ? 1 : 0);
  // assert(i mod 2 != 0 && j mod 2 != 0);
  let (ts, (ts_l, ts_r)) = Lists.Framed.sublist(i, j + 1, ts);
  let (cs, (cs_l, cs_r)) = Lists.Framed.sublist(i, j, cs);
  (
    Chain.mk(cs_l @ [l], ts_l),
    Wald.mk(ts, cs),
    Chain.mk(cs_r @ [r], ts_r),
  );
};

let length = m => Chain.length(to_chain(m));

let unzip_cell = (step, m) => Chain.unzip_loop(step, to_chain(m));
let unzip_tok = (step, m) => Chain.unzip_link(step, to_chain(m));
let unzip = (step, m) => Chain.unzip(step, to_chain(m));

let link = (~cell=Cell.empty, t: Token.t, M(l, W(w), r): t) =>
  M(cell, W(Chain.link(t, l, w)), r);

let map_cell = (_, _) => failwith("todo Meld.map_cell");

let rev = (M(l, W(w), r): t) => M(r, W(Chain.rev(w)), l);

let face = (~side: Dir.t, M(_, w, _)) => Wald.face(~side, w);

let is_space =
  fun
  | M(_, W(([{mtrl: Space (), _} as tok], [])), _) => Some(tok)
  | _ => None;

let map_cells = (f, M(l, W((toks, cells)), r)) => {
  let (l, r) = (f(l), f(r));
  let cells = List.map(f, cells);
  M(l, W((toks, cells)), r);
};
