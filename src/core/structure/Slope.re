open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('tok) = list(Terr.Base.t('tok));
  let empty = [];
  let singleton = t => [t];
  let height = List.length;
  let cons = List.cons;
  let cat = (@);
};
include Base;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Base.t(Token.t);

let tokens = List.concat_map(Terr.tokens);
// let link = (w: Wald.t, c: Rel.t(_), slope: t) =>
//   switch (c) {
//   | Neq(c) => [Terr.Base.{cell: c, wald: Wald.rev(w)}, ...slope]
//   | Eq(c) =>
//     switch (slope) {
//     | [] => [Terr.Base.{cell: c, wald: Wald.rev(w)}]
//     | [hd, ...tl] => [{...hd, wald: Wald.zip_cell(w, c, hd.wald)}, ...tl]
//     }
//   };

let link = (tok: Token.t, cell: Cell.t) =>
  fun
  | [] => raise(Invalid_argument("Slope.link"))
  | [hd, ...tl] => [Terr.link(tok, cell, hd), ...tl];
let unlink =
  fun
  | [] => None
  | [hd, ...tl] => {
      let (tok, cell, rest) = Terr.unlink(hd);
      let rest =
        switch (rest) {
        | None => tl
        | Some(hd) => [hd, ...tl]
        };
      Some((tok, cell, rest));
    };

let face =
  fun
  | [] => Delim.root
  | [hd, ..._] => Delim.tok(Terr.face(hd));
let map_face = f =>
  fun
  | [] => None
  | [hd, ...tl] => Some([Terr.map_face(f, hd), ...tl]);

// let extend = tl =>
//   fun
//   | [] => None
//   | [hd, ...rest] => Some([Terr.extend(tl, hd), ...rest]);
let extend = (tl: Chain.Affix.t(Cell.t, Token.t)) =>
  fun
  | [] => raise(Invalid_argument("Slope.extend"))
  | [hd, ...rest] => [Terr.extend(tl, hd), ...rest];

let fold: (('acc, Terr.t) => 'acc, 'acc, t) => 'acc = List.fold_left;

let unroll = (~from: Dir.t, cell: Cell.t) => {
  let rec go = (cell: Cell.t, unrolled) =>
    switch (Cell.get(cell)) {
    | None => (cell, unrolled)
    | Some(M(l, w, r)) =>
      let (cell, terr) =
        switch (from) {
        | L => (r, Terr.Base.{wald: Wald.rev(w), cell: l})
        | R => (l, Terr.Base.{wald: w, cell: r})
        };
      go(cell, [terr, ...unrolled]);
    };
  go(cell, []);
};
let rec roll = (~onto: Dir.t, ~fill=Cell.empty, slope: t) =>
  switch (slope) {
  | [] => fill
  | [hd, ...tl] =>
    let m =
      switch (onto) {
      | L => Meld.mk(~l=hd.cell, Wald.rev(hd.wald), ~r=fill)
      | R => Meld.mk(~l=fill, hd.wald, ~r=hd.cell)
      };
    roll(~onto, ~fill=Cell.put(m), tl);
  };

let pull_terr = (~from: Dir.t, terr: Terr.t): (Token.t, t) => {
  let (tok, rest) = Wald.uncons(terr.wald);
  let slope =
    switch (rest) {
    | ([], _) => snd(unroll(~from, terr.cell))
    | ([cell, ...cells], toks) =>
      let terr = {...terr, wald: Wald.mk(toks, cells)};
      cat(snd(unroll(~from, cell)), [terr]);
    };
  (tok, slope);
};

// here "from" indicates which side slope is relative to puller
// eg "pull from dn slope on left"
let pull = (~from: Dir.t, slope: t): (Delim.t, t) =>
  switch (slope) {
  | [] => (Delim.root, slope)
  | [hd, ...tl] =>
    let (tok, slope) = pull_terr(~from, hd);
    (Delim.tok(tok), cat(slope, tl));
  };

let merge_hd = (~onto: Dir.t, t: Token.t, slope: t): option(t) =>
  switch (slope) {
  | [] => None
  | [hd, ...tl] =>
    Terr.merge_hd(~onto, t, hd) |> Option.map(hd => [hd, ...tl])
  };

// Dn and Up slopes named based on left-to-right order of terraces
// as displayed on screen, but terraces are always maintained
// in list order low-to-high
module Dn = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(Terr.R.t);
  // let flatten = List.concat_map(Terr.R.flatten);
  let roll = roll(~onto=L);
  let unroll = unroll(~from=L);
  let pull = pull(~from=L);
};
module Up = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(Terr.L.t);
  // let flatten = List.concat_map(Terr.L.flatten);
  let roll = roll(~onto=R);
  let unroll = unroll(~from=R);
  let pull = pull(~from=R);
};
