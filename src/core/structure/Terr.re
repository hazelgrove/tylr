open Stds;

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    cell: Cell.t,
    wald: Wald.t,
  };
};
include Base;

let mk = (toks, cells) => {
  let (cells, cell) =
    Stds.Lists.Framed.ft(cells)
    |> Options.get_exn(Invalid_argument("Terr.mk"));
  {wald: Wald.mk(toks, List.rev(cells)), cell};
};
let mk' = ((toks, cells)) =>
  switch (mk(toks, cells)) {
  | terr => Some(terr)
  | exception (Invalid_argument(_)) => None
  };

let unmk = ({wald: W((toks, cells)), cell}: t) => (toks, cells @ [cell]);

let hd = terr => Wald.hd(terr.wald);
let tokens = terr => Wald.tokens(terr.wald);

let sort = (terr: t) => Wald.sort(terr.wald);
let face = (terr: t) => Wald.face(terr.wald);
let cells = (terr: t) => Wald.cells(terr.wald) @ [terr.cell];

let of_tok = tok => {cell: Cell.empty, wald: Wald.of_tok(tok)};

let link = (t, c, terr: t) => {...terr, wald: Wald.link(t, c, terr.wald)};
let extend = (tl, terr: t) => {...terr, wald: Wald.extend(tl, terr.wald)};

let unlink = (terr: t) =>
  switch (Wald.unlink(terr.wald)) {
  | Ok((tok, cell, wald)) => (tok, cell, Some({...terr, wald}))
  | Error(tok) => (tok, terr.cell, None)
  };

module Tl = {
  // a terrace minus its hd token
  type t = Chain.t(Cell.t, Token.t);
};

let uncons = (terr: t): (Token.t, Tl.t) =>
  Wald.uncons(terr.wald) |> Tuples.map_snd(Chain.snoc(terr.cell));

module L = {
  // L2R: wald cell
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Base.t;
};
module R = {
  // L2R: cell wald
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Base.t;
};
