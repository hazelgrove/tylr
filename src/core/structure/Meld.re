open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Util;

module Cell = {
  [@deriving (sexp, yojson)]
  type t('meld) = {
    marks: Path.Marks.t,
    meld: option('meld),
  };
  let mk = (~marks=Path.Marks.empty, ~meld=?, ()) => {marks, meld};
  let empty = mk();
  let pp = (pp_meld, out, {marks, meld}: t(_)) =>
    Path.Marks.is_empty(marks) && Option.is_none(meld)
      ? Fmt.pf(out, "{}")
      : Fmt.pf(
          out,
          "{%a@ |@ %a}",
          Path.Marks.pp,
          marks,
          Fmt.option(pp_meld),
          meld,
        );
  let show = pp_meld => Fmt.to_to_string(pp(pp_meld));
};

module Wald = {
  [@deriving (sexp, yojson)]
  type t('cell) =
    | W(Chain.t(Token.t, 'cell));
  let mk = (toks: list(_), cells: list(Cell.t(_))) =>
    W(Chain.mk(toks, cells));
  let of_tok = tok => W(Chain.unit(tok));
  let face = (~side=Dir.L, W(w): t(_)) => {
    let tok = Dir.pick(side, (Chain.hd, Chain.ft), w);
    (tok.mtrl, tok.mold);
  };
  let pp = (pp_cell, out, W(w): t(_)) => {
    let pp_hd = Token.pp;
    let pp_tl = Fmt.(list(~sep=sp, pair(~sep=sp, pp_cell, Token.pp)));
    let pp = Fmt.(pair(~sep=sp, pp_hd, pp_tl));
    let (t, (cs, ts)) = Chain.split_hd(w);
    pp(out, (t, List.combine(cs, ts)));
  };
  let show = pp_cell => Fmt.to_to_string(pp(pp_cell));
};

module Base = {
  [@deriving (sexp, yojson)]
  type t =
    | M(Cell.t(t), Wald.t(Cell.t(t)), Cell.t(t));
  let rec pp = (out, M(l, w, r): t) => {
    let pp_cell = Cell.pp(pp);
    let pp_wald = Wald.pp(pp_cell);
    Fmt.pf(out, "%a@ %a@ %a", pp_cell, l, pp_wald, w, pp_cell, r);
  };
  let show = Fmt.to_to_string(pp);
};
include Base;

let mk = (~l=Cell.empty, ~r=Cell.empty, w) => M(l, w, r);
let of_tok = (~l=Cell.empty, ~r=Cell.empty, tok) =>
  mk(~l, Wald.of_tok(tok), ~r);

let length = (M(_, W(w), _): t) => Chain.length(w);

let is_empty =
  fun
  | M({meld: None, _}, W(([tok], [])), {meld: None, _}) =>
    Token.is_empty(tok)
  | _ => false;

let to_chain = (M(l, W((ts, cs)), r): t) => ([l, ...cs] @ [r], ts);
let of_chain = ((cs, ts): Chain.t(Cell.t(_), Token.t)) => {
  let get = Options.get_exn(Invalid_argument("Meld.of_chain"));
  // cs reversed twice
  let (cs, r) = get(Lists.Framed.ft(cs));
  let (cs, l) = get(Lists.Framed.ft(cs));
  mk(~l, W((ts, cs)), ~r);
};

let fold = (f_hd, f_tl, m: t) => Chain.fold_left(f_hd, f_tl, to_chain(m));

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
    | M(_, W(([tok], [])), _) when Token.Space.is(tok) => Some(tok)
    | _ => None;
};
module Grout = {
  let op_ = (s: Mtrl.Sorted.t) => mk(Wald.of_tok(Token.Grout.op_(s)));
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

let size = m => Chain.length(to_chain(m));

let unzip_cell = (step, m) => Chain.unzip_loop(step, to_chain(m));
let unzip_tok = (step, m) =>
  Chain.unzip_link(step, to_chain(m))
  |> Options.get_fail("impossible: meld has at least one token");

let link = (~cell=Cell.empty, t: Token.t, M(l, W(w), r): t) =>
  M(cell, W(Chain.link(t, l, w)), r);

let map_cell = (_, _) => failwith("todo Meld.map_cell");

let rev = (M(l, W(w), r): t) => M(r, W(Chain.rev(w)), l);

let face = (~side: Dir.t, M(_, w, _)) => Wald.face(~side, w);

let is_space =
  fun
  | M(_, W(([{mtrl: Space, _} as tok], [])), _) => Some(tok)
  | _ => None;

let map_cells = (f, M(l, W((toks, cells)), r)) => {
  let (l, r) = (f(l), f(r));
  let cells = List.map(f, cells);
  M(l, W((toks, cells)), r);
};
