open Stds;

module Base = {
  include Cell.Wald;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('tok) = Cell.Wald.t('tok, Cell.Base.t('tok));
  let w = (c): t(_) => W(c);
  let mk = (toks, cells): t(_) => W(Chain.mk(toks, cells));
  let hd = (W(w): t(_)) => Chain.hd(w);
  let ft = (W(w): t(_)) => Chain.ft(w);
  let length = (W(c): t(_)) => Chain.length(c);
  let rev = (W(c): t(_)) => w(Chain.rev(c));
};
include Base;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Base.t(Token.t);

let get = (f, W(w): t) => f(w);
let map = (f, W(w): t): t => W(f(w));

let uncons: t => _ = get(Chain.uncons);
let map_hd = f => map(Chain.map_hd(f));
let put_hd = hd => map(Chain.put_hd(hd));
let put_ft = ft => map(Chain.put_ft(ft));
// let fst = (W(w)) => Chain.hd(w);
// let ft = (W(w)) => Chain.ft(w);
// let face =
//   fun
//   | Dir.L => fst
//   | R => ft;

let link = (tok, cell) => map(Chain.link(tok, cell));
let unlink = (W(w): t) =>
  Chain.unlink(w)
  |> Result.map(~f=((tok, cell, (ts, cs))) => (tok, cell, mk(ts, cs)));

let tokens: t => list(Token.t) = get(Chain.loops);
let cells: t => list(Cell.t) = get(Chain.links);

let is_grout = (w: t): option(list(Token.Grout.t)) =>
  tokens(w) |> List.map(Token.Grout.is_) |> Options.for_all;

let extend = tl => map(Chain.extend(tl));

let sort = w =>
  Token.sort(hd(w))
  |> Mtrl.map(~space=Fun.const(), ~grout=Fun.id, ~tile=Fun.id);

let fold = (f, g, W(w): t) => Chain.fold_left(f, g, w);

// let flatten = (W(w): t) =>
//   w |> Chain.to_list(Lists.single, Cell.flatten) |> List.concat;

module Affix = {
  include Chain.Affix;
  type t = Chain.Affix.t(Cell.t, Token.t);
};

let unzip_tok = (n, W(w): t) => Chain.unzip_loop(n, w);
let zip_tok = (~pre=Affix.empty, ~suf=Affix.empty, tok) =>
  w(Chain.zip(~pre, tok, ~suf));

let unzip_cell = (n, W((toks, cells)): t) => {
  let (tok, (toks_l, toks_r)) = Lists.Framed.nth_exn(n, toks);
  let (cell, (cs_l, cs_r)) = Lists.Framed.nth_exn(n, cells);
  (mk([tok, ...toks_l], cs_l), cell, mk(toks_r, cs_r));
};
let zip_cell = (pre: t, cell: Cell.t, suf: t) =>
  pre
  |> fold(
       tok => link(tok, cell, suf),
       (zipped, cell, tok) => link(tok, cell, zipped),
     );

let merge_hds = (~save_cursor=false, ~from: Dir.t, src: t, dst: t): option(t) => {
  let (hd_src, tl_src) = uncons(src);
  let (hd_dst, tl_dst) = uncons(dst);
  let (hd_l, hd_r) = Dir.order(from, (hd_src, hd_dst));
  let (tl_l, tl_r) = Dir.order(from, (tl_src, tl_dst));
  let save_cursor = save_cursor ? Some(Dir.toggle(from)) : None;
  Token.merge(~save_cursor?, hd_l, hd_r)
  |> Option.map(tok => w(Chain.zip(~pre=tl_l, tok, ~suf=tl_r)));
};
