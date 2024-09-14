open Stds;

module Base = Cell.Meld;
include Base;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Base.t(Cell.t, Token.t);

let mk = (~l=Cell.empty, ~r=Cell.empty, w) => Base.mk(~l, w, ~r);
let of_tok = (~l=Cell.empty, ~r=Cell.empty, tok) =>
  mk(~l, Wald.of_tok(tok), ~r);

// let is_empty =
//   fun
//   | M({meld: None, _}, W(([tok], [])), {meld: None, _}) =>
//     Token.is_empty(tok)
//   | _ => false;

let tokens = (M(_, W((toks, _)), _): t) => toks;

let fold = (f_hd, f_tl, m: t) => Chain.fold_left(f_hd, f_tl, to_chain(m));

// let rec flatten = (m: t): list(Token.t) =>
//   to_chain(m)
//   |> Chain.to_list(
//        (c: Cell.t(t)) =>
//          c.meld |> Option.map(flatten) |> Option.to_list |> List.flatten,
//        Lists.single,
//      )
//   |> List.flatten;

module Affix = {
  include Chain.Affix;
  type t = Chain.Affix.t(Token.t, Cell.t);
};

module Space = {
  let mk = (tok: Token.t) => {
    assert(Mtrl.is_space(tok.mtrl));
    mk(Wald.of_tok(tok));
  };
  let get = Base.get_space;
  let is = m => Option.is_some(get(m));
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

let map_cells = (f, M(l, W((toks, cells)), r)) => {
  let (l, r) = (f(l), f(r));
  let cells = List.map(f, cells);
  M(l, W((toks, cells)), r);
};
