open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Util;

module Cell = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('meld) = {
    marks: Path.Marks.t,
    meld: option('meld),
  };
  let mk = (~marks=Path.Marks.empty, ~meld=?, ()) => {marks, meld};
  let empty = mk();
};

module Wald = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('cell) =
    | W(Chain.t(Token.t, 'cell));
  let mk = (toks: list(_), cells: list(Cell.t(_))) =>
    W(Chain.mk(toks, cells));
  let of_tok = tok => W(Chain.unit(tok));
  let face = (~side=Dir.L, W(w): t(_)) => {
    let tok = Dir.pick(side, (Chain.hd, Chain.ft), w);
    (tok.mtrl, tok.mold);
  };
};

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | M(Cell.t(t), Wald.t(Cell.t(t)), Cell.t(t));
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

let split_subwald = (_, _, _) => failwith("todo Meld.split_subwald");

let size = m => Chain.size(to_chain(m));

let unzip_cell = (step, m) => Chain.unzip_loop(step, to_chain(m));
let unzip_tok = (step, m) =>
  Chain.unzip_link(step, to_chain(m))
  |> OptUtil.get_or_fail("impossible: meld has at least one token");

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
