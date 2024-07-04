let zip_lt = (zipped: Cell.t, r: Terr.L.t) =>
  Cell.put(M(zipped, r.wald, r.cell));
let zip_gt = (l: Terr.R.t, zipped: Cell.t) =>
  Cell.put(M(l.cell, Wald.rev(l.wald), zipped));
let zip_eq = (l: Terr.R.t, zipped: Cell.t, r: Terr.L.t) => {
  let w = Wald.zip_cell(l.wald, zipped, r.wald);
  Cell.put(Meld.mk(~l=l.cell, w, ~r=r.cell));
};

module Open = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (Slope.Dn.t, Slope.Up.t);
  let empty = Slope.(empty, empty);
  let cons = (~onto: Dir.t, terr: Terr.t, (dn, up)) =>
    switch (onto) {
    | L => ([terr, ...dn], up)
    | R => (dn, [terr, ...up])
    };
  let cat = ((dn', up'), (dn, up)) => Slope.(cat(dn', dn), cat(up', up));
  let face = (~side: Dir.t, (dn, up): t) =>
    Slope.face(Dir.pick(side, (dn, up)));
  let extend = (~side: Dir.t, tl, (dn, up): t) =>
    switch (side) {
    | L => (Slope.extend(tl, dn), up)
    | R => (dn, Slope.extend(tl, up))
    };
  let zips =
    fun
    | ([hd_l, ..._], [hd_r, ..._])
        when Option.is_some(Token.zip(Wald.hd(hd_l), Wald.hd(hd_r))) =>
      true
    | _ => false;
  let zip_toks = (~caret=?) =>
    fun
    | (([hd_l, ...tl_l], [hd_r, ...tl_r]): t) =>
      Wald.zip_hds(~from=L, hd_l.wald, ~caret?, hd_r.wald)
      |> Option.map(w => (Meld.M(hd_l.cell, w, hd_r.cell), (tl_l, tl_r)))
    | _ => None;

  let zip_step = (~zipped: Cell.t, (dn, up): t) =>
    switch (dn, up) {
    | ([], []) => None
    | ([], [hd, ...tl]) =>
      Some((Rel.Neq(Dir.L), zip_lt(zipped, hd), (dn, tl)))
    | ([hd, ...tl], []) => Some((Neq(R), zip_gt(hd, zipped), (tl, up)))
    | ([l, ...dn], [r, ...up])
        when Option.is_some(Token.zip(Terr.hd(l), Terr.hd(r))) =>
      let caret = Cell.is_caret(zipped);
      let w = Option.get(Wald.zip_hds(~from=L, l.wald, ~caret?, r.wald));
      Some((Eq(), Cell.put(M(l.cell, w, r.cell)), (dn, up)));
    | ([l, ..._], [r, ...up]) when Melder.lt(l.wald, r.wald) =>
      Some((Neq(L), zip_lt(zipped, r), (dn, up)))
    | ([l, ...dn], [r, ..._]) when Melder.gt(l.wald, r.wald) =>
      Some((Neq(R), zip_gt(l, zipped), (dn, up)))
    | ([l, ...dn], [r, ...up]) =>
      assert(Melder.eq(l.wald, r.wald));
      Some((Eq(), zip_eq(l, zipped, r), (dn, up)));
    };

  let rec zip = (~zipped: Cell.t, (dn, up): t) =>
    switch (zip_step(~zipped, (dn, up))) {
    | None => zipped
    | Some((_, zipped, rest)) => zip(~zipped, rest)
    };
};

module Closed = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (Terr.R.t, Terr.L.t);
  let zip = (~zipped: Cell.t, (l, r): t) => zip_eq(l, zipped, r);
};
