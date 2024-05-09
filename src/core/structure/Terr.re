open Util;

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
    Util.Lists.Framed.ft(cells)
    |> Options.get_exn(Invalid_argument("Terr.mk"));
  {wald: Wald.mk(toks, List.rev(cells)), cell};
};
let mk' = ((toks, cells)) =>
  switch (mk(toks, cells)) {
  | terr => Some(terr)
  | exception (Invalid_argument(_)) => None
  };

let sort = (terr: t) => Wald.sort(terr.wald);
let face = (terr: t) => Wald.face(terr.wald);
let cells = (terr: t) => Wald.cells(terr.wald) @ [terr.cell];

let of_tok = tok => {cell: Cell.empty, wald: Wald.of_tok(tok)};

let link = (t, c, terr: t) => {...terr, wald: Wald.link(t, c, terr.wald)};
let extend = (tl, terr: t) => {...terr, wald: Wald.extend(tl, terr.wald)};

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
