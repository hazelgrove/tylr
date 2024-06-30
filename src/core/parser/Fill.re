open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Chain.t(Cell.t, unit);

let empty = Chain.unit(Cell.empty);
let is_empty =
  fun
  | ([c], []) => Cell.is_empty(c)
  | _ => false;
let unit = Chain.unit;
let hd = Chain.hd;
let to_list = Chain.loops;
let length = fill => List.length(to_list(fill));
let rev = fill => Chain.rev(fill);
let cons = (c: Cell.t, fill: t) => Chain.link(c, (), fill);

let face = (~side: Dir.t, fill: t) => {
  let c = Dir.pick(side, Chain.(hd, ft), fill);
  Cell.face(~side, c);
};

let is_space = (fill: t) =>
  switch (Chain.unlink(fill)) {
  | Error(cell) when Cell.Space.is_space(cell) => Some(cell)
  | _ => None
  };

let is_caret = (fill: t) =>
  switch (Chain.unlink(fill)) {
  | Error(cell) => Cell.is_caret(cell)
  | _ => None
  };

// returns none if spc is not space
// todo: make this less weird
let rec pad = (~side: Dir.t, ~spc: Cell.t, c: Cell.t): option(Cell.t) => {
  open Options.Syntax;
  let/ () = {
    let (l, r) = Dir.order(side, (spc, c));
    Cell.Space.merge(l, r);
  };
  let* M(l, w, r) = Cell.get(c);
  switch (side) {
  | L =>
    let+ l = pad(~side, ~spc, l);
    Cell.put(M(l, w, r));
  | R =>
    let+ r = pad(r, ~spc, ~side);
    Cell.put(M(l, w, r));
  };
};

let squash = (fill: t) => {
  let rec go = (~pre=Chain.Affix.empty, fill) =>
    switch (Chain.Affix.unlink(pre), Chain.unlink(fill)) {
    | (None, Error(_)) => fill
    | (None, Ok((cell, (), fill))) =>
      go(~pre=Chain.Affix.link((), cell, pre), fill)
    | (Some(((), cell, pre_tl)), Error(hd)) =>
      switch (pad(~side=L, ~spc=cell, hd), pad(cell, ~spc=hd, ~side=R)) {
      | (None, None) => Chain.extend(pre, fill)
      | (Some(padded), _)
      | (_, Some(padded)) => Chain.extend(pre_tl, Chain.unit(padded))
      }
    | (Some(((), cell, pre_tl)), Ok((hd, (), tl))) =>
      switch (pad(~side=L, ~spc=cell, hd), pad(cell, ~spc=hd, ~side=R)) {
      | (None, None) => go(~pre=Chain.Affix.link((), hd, pre), tl)
      | (Some(padded), _)
      | (_, Some(padded)) =>
        go(~pre=Chain.Affix.link((), padded, pre_tl), tl)
      }
    };
  go(fill);
};

let default =
  fun
  | Mtrl.Space(_) => Cell.empty
  // grout case isn't quite right... but shouldn't arise
  | Grout(s)
  | Tile((s, _)) => Cell.put(Meld.Grout.op_(s));
