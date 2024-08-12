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
  let pull = (~from: Dir.t, (dn, up): t): (Delim.t, t) =>
    switch (from) {
    | L =>
      let (l, dn) = Slope.pull(~from=L, dn);
      (l, (dn, up));
    | R =>
      let (r, up) = Slope.pull(~from=R, up);
      (r, (dn, up));
    };

  let extend = (~side: Dir.t, tl, (dn, up): t) =>
    switch (side) {
    | L => (Slope.extend(tl, dn), up)
    | R => (dn, Slope.extend(tl, up))
    };
  // todo: rename this to merge_faces
  let zip_toks = (~save_cursor=false) =>
    fun
    | (([hd_l, ...tl_l], [hd_r, ...tl_r]): t) =>
      Wald.merge_hds(~save_cursor, ~from=L, hd_l.wald, hd_r.wald)
      |> Option.map(w => (Meld.M(hd_l.cell, w, hd_r.cell), (tl_l, tl_r)))
    | _ => None;

  let zip_step = (~save_cursor, ~zipped: Cell.t, (dn, up): t) =>
    switch (dn, up) {
    | ([], []) => None
    | ([], [hd, ...tl]) =>
      Some((Rel.Neq(Dir.L), zip_lt(zipped, hd), (dn, tl)))
    | ([hd, ...tl], []) => Some((Neq(R), zip_gt(hd, zipped), (tl, up)))
    | ([l, ...dn], [r, ...up])
        when Option.is_some(Token.merge(Terr.hd(l), Terr.hd(r))) =>
      let w =
        Option.get(Wald.merge_hds(~save_cursor, ~from=L, l.wald, r.wald));
      Some((Eq(), Cell.put(M(l.cell, w, r.cell)), (dn, up)));
    | ([l, ..._], [r, ...up]) when Melder.lt(l.wald, r.wald) =>
      Some((Neq(L), zip_lt(zipped, r), (dn, up)))
    | ([l, ...dn], [r, ..._]) when Melder.gt(l.wald, r.wald) =>
      Some((Neq(R), zip_gt(l, zipped), (dn, up)))
    | ([l, ...dn], [r, ...up]) =>
      assert(Melder.eq(l.wald, r.wald));
      Some((Eq(), zip_eq(l, zipped, r), (dn, up)));
    };

  let rec zip = (~save_cursor=false, ~zipped: Cell.t, (dn, up): t) =>
    switch (zip_step(~save_cursor, ~zipped, (dn, up))) {
    | None => zipped
    | Some((_, zipped, rest)) => zip(~zipped, rest)
    };
};

module Closed = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (Terr.R.t, Terr.L.t);
  let zip = (~zipped: Cell.t, (l, r): t) => zip_eq(l, zipped, r);
  let map_face = (~side: Dir.t, f, (l, r): t) =>
    switch (side) {
    | L => Terr.map_face(f, l) |> (l => (l, r))
    | R => Terr.map_face(f, r) |> (r => (l, r))
    };
  let pull = (~from: Dir.t, (l, r): t): (Token.t, Open.t) =>
    switch (from) {
    | L =>
      let (l, dn) = Slope.pull_terr(~from=L, l);
      (l, (dn, [r]));
    | R =>
      let (r, up) = Slope.pull_terr(~from=R, r);
      (r, ([l], up));
    };
};
