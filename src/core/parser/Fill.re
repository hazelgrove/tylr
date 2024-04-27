open Util;

type t = Chain.t(Cell.t, unit);

let empty = Chain.unit(Cell.empty);
let is_empty: t => bool = (==)(empty);

let unit = Chain.unit;

let hd = Chain.hd;

// let init = (c: Cell.t) =>
//   switch (Cell.get(c)) {
//   | None => empty
//   | Some(m) => [m]
//   };

let to_list = Chain.loops;
let length = fill => List.length(to_list(fill));

let faces = (fill: t) => {
  let (hd, ft) = Chain.(hd(fill), ft(fill));
  Cell.(face(~side=L, hd), face(ft, ~side=R));
};

let is_space = (fill: t) =>
  switch (Chain.unlink(fill)) {
  | Error(cell) when Cell.Space.is_space(cell) => Some(cell)
  | _ => None
  };

let cons = (c: Cell.t, fill: t) => Chain.link(c, (), fill);
let rev = fill => Chain.rev(fill);

let rec pad = (~side: Dir.t, ~spc: Cell.t, c: Cell.t): option(Cell.t) => {
  open OptUtil.Syntax;
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

let get_space =
  fun
  | [] => Some(Token.Space.empty)
  | [c] => Cell.Space.get(c)
  | [_, ..._] => None;

let rec pad_meld = (~side as d: Dir.t, spc: Token.t, m: Meld.t) => {
  let _ = failwith("todo: change padding to be cell for cursors");
  switch (Meld.Space.get(m)) {
  | Some(spc') =>
    let (l, r) = Dir.order(d, (spc, spc'));
    let spc = Token.merge(l, r);
    Meld.Space.mk(spc);
  | None =>
    let M(l, w, r) = m;
    let (c_d, c_b) = Dir.order(d, (l, r));
    let c_d = pad_cell(~side=d, spc, c_d);
    let (l, r) = Dir.order(d, (c_d, c_b));
    Meld.M(l, w, r);
  };
}
and pad_cell = (~side: Dir.t, spc: Token.t, c: Cell.t) =>
  Cell.put(
    switch (Cell.get(c)) {
    | None => Meld.Space.mk(spc)
    | Some(m) => pad_meld(~side, spc, m)
    },
  );

// let padding = (nt: Bound.t(Molded.NT.t)) =>
//   nt |> Bound.map(Molded.NT.padding) |> Bound.get(~root=Padding.root);

let fill_default = (nt: Bound.t(Molded.NT.t)) =>
  switch (Molded.NT.mtrl(nt)) {
  | Space => Cell.empty
  | (Grout | Tile(_)) as s => Cell.put(Meld.Grout.op_(s))
  };

// assumes precedence-correctness already checked
// and that fill has been oriented
let fill = (~l=false, ~r=false, fill: t, nt: Bound.t(Molded.NT.t)): Cell.t => {
  let fill = squash(fill);
  switch (is_space(fill)) {
  | Some(spc) =>
    fill_default(nt)
    |> pad(~side=L, ~spc)
    |> OptUtil.get_or_fail("todo: shouldn't be possible")
  | None =>
    assert(!is_empty(fill));
    let s = Molded.NT.mtrl(nt);
    let cells =
      [l ? [Cell.empty] : [], to_list(fill), r ? [Cell.empty] : []]
      |> List.concat;
    let toks =
      Token.Grout.[
        l ? [pre(s)] : [],
        List.init(length(fill) - 1, _ => in_(s)),
        r ? [pos(s)] : [],
      ]
      |> List.concat;
    switch (toks) {
    | [] => List.hd(cells)
    | [_, ..._] =>
      let (l, cells) = ListUtil.split_first(cells);
      let (cells, r) = ListUtil.split_last(cells);
      Cell.put(M(l, Wald.mk(toks, cells), r));
    };
  };
  // switch (get_space(fill)) {
  // | Some(spc) => fill_default(nt) |> pad_cell(~side=L, spc)
  // | None =>
  //   assert(!is_empty(fill));
  //   let s = Molded.NT.mtrl(nt);
  //   let cells =
  //     [l ? [Cell.empty] : [], fill, r ? [Cell.empty] : []] |> List.concat;
  //   let toks =
  //     Token.Grout.[
  //       l ? [pre(s)] : [],
  //       List.init(List.length(fill) - 1, _ => in_(s)),
  //       r ? [pos(s)] : [],
  //     ]
  //     |> List.concat;
  //   switch (toks) {
  //   | [] => List.hd(cells)
  //   | [_, ..._] =>
  //     let (l, cells) = ListUtil.split_first(cells);
  //     let (cells, r) = ListUtil.split_last(cells);
  //     Cell.put(M(l, Wald.mk(toks, cells), r));
  //   };
  // };
};
