open Stds;

include Meld.Wald;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Meld.Wald.t(Cell.t);

let get = (f, W(w)) => f(w);
let map = (f, W(w)) => W(f(w));

let uncons: t => _ = get(Chain.uncons);
let hd: t => Token.t = get(Chain.hd);
let put_hd = hd => map(Chain.put_hd(hd));

let ft = _ => failwith("todo Wald.ft");
let put_ft = (_, _) => failwith("todo Wald.put_ft");
// let fst = (W(w)) => Chain.hd(w);
// let ft = (W(w)) => Chain.ft(w);
// let face =
//   fun
//   | Dir.L => fst
//   | R => ft;

let length: t => int = get(Chain.length);
let rev: t => t = map(Chain.rev);

let link = (tok, cell) => map(Chain.link(tok, cell));
let unlink = (W(w)) =>
  Chain.unlink(w)
  |> Result.map(~f=((tok, cell, tl)) => (tok, cell, W(tl)));

let tokens: t => list(Token.t) = get(Chain.loops);
let cells: t => list(Cell.t) = get(Chain.links);

let extend = tl => map(Chain.extend(tl));

let sort = w => Token.sort(hd(w));

let fold = (f, g, W(w)) => Chain.fold_left(f, g, w);

let flatten = (W(w): t) =>
  w |> Chain.to_list(Lists.single, Cell.flatten) |> List.concat;

module Affix = {
  include Chain.Affix;
  type t = Chain.Affix.t(Cell.t, Token.t);
};

let unzip_tok = (n, W(w)) => Chain.unzip_loop(n, w);
let zip_tok = (~pre=Affix.empty, ~suf=Affix.empty, tok) =>
  W(Chain.zip(~pre, tok, ~suf));

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

let zip_hds = (~from: Dir.t, src: t, ~caret=?, dst: t): option(t) => {
  // add caret mark
  let (src, dst) =
    switch (caret) {
    | None => (src, dst)
    | Some(caret) =>
      let p = Step.Caret.mk(caret, 0);
      switch (from) {
      | L => (src, map(Chain.map_hd(Token.add_mark(p)), dst))
      | R => (map(Chain.map_hd(Token.add_mark(p)), src), dst)
      };
    };
  let (hd_src, tl_src) = uncons(src);
  let (hd_dst, tl_dst) = uncons(dst);
  let (hd_l, hd_r) = Dir.order(from, (hd_src, hd_dst));
  let (tl_l, tl_r) = Dir.order(from, (tl_src, tl_dst));
  Token.zip(hd_l, hd_r)
  |> Option.map(tok => W(Chain.zip(~pre=tl_l, tok, ~suf=tl_r)));
};
