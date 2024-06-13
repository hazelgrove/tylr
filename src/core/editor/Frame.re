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
};

module Closed = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (Terr.R.t, Terr.L.t);
};
