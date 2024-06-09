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
};

module Closed = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (Terr.R.t, Terr.L.t);
};

let mold =
    (tok: Token.t, ~fill=Cell.empty, (dn, up): Open.t, bound: Bound.t(Closed.t))
    : Rel.t((Terr.R.t, Slope.Up.t), Open.t) => {
  let (l, r) = Bound.split(bound);
  Molder.mold(~bound=l, dn, ~fill, Token.unmold(tok))
  |> Rel.map(
    ~neq=dn => (dn, up),
    ~eq=terr => (terr, up @ Bound.to_list(r)),
  );
};
let remold_wald = (wald: Wald.t, ~fill=Cell.empty, (dn, up), bound) => {
  let (hd, tl) = Wald.uncons(wald);
  let molded = mold(hd, ~fill, (dn, up), bound);
  if (Melded.face(molded) == hd.mtrl)
}

let rec remold =
        (~fill=Cell.empty, (dn, up): Open.t, bound: Bound.t(Closed.t))
        : Rel.t((Terr.R.t, Cell.t, Slope.Up.t), Cell.t) =>
  switch (Slope.unlink(up)) {
  | Some((tok, cell, up)) when Token.Grout.is(tok) =>
    Effects.remove(tok);
    let up = Slope.cat(Slope.Up.unroll(cell), up);
    remold(~fill, (dn, up), bound);
  | _ =>
    switch (up) {
    | [] => Ok(Melder.complete_slope(~onto=L, dn, ~fill))
    | [hd, ...tl] =>
      let (tok, rest) = Wald.split_hd(hd.wald);
      let (l, r) = Bound.(map(fst, bound), map(snd, bound));
      let molded = mold(~bound=l, dn, ~fill, Token.Unmolded.unmold(tok));
      let (molded, fill, up) =
        if (Rel.get(Terr.face, Slope.face, molded) == tok.mtrl) {
          // fast path for when tok retains original mold
          let extended =
            molded |> Rel.map(Terr.extend(rest), Slope.extend(rest));
          (extended, hd.cell, tl);
        } else {
          let (cell, up) =
            switch (Chain.Affix.split_hd(rest)) {
            | None => (hd.cell, tl)
            | Some((cell, (ts, cs))) =>
              let hd = {...hd, wald: Wald.mk(ts, cs)};
              (cell, [hd, ...tl]);
            };
          (molded, Cell.empty, Slope.(cat(Up.unroll(cell), up)));
        };
      switch (molded) {
      | Eq(l) => Eq((l, fill, Slope.cat(up, Bound.to_list(r))))
      | Neq(dn) => remold(~fill, (dn, up), bound)
      };
    }
  };


let rec remold =
        (~fill=Cell.empty, (dn, up): Open.t, bound: Bound.t(Closed.t))
        : Rel.t((Terr.R.t, Cell.t, Slope.Up.t), Cell.t) =>
  switch (Slope.unlink(up)) {
  | Some((tok, cell, up)) when Token.Grout.is(tok) =>
    Effects.remove(tok);
    let up = Slope.cat(Slope.Up.unroll(cell), up);
    remold(~fill, (dn, up), bound);
  | _ =>
    switch (up) {
    | [] => Ok(Melder.complete_slope(~onto=L, dn, ~fill))
    | [hd, ...tl] =>
      let (tok, rest) = Wald.split_hd(hd.wald);
      let (l, r) = Bound.(map(fst, bound), map(snd, bound));
      let molded = mold(~bound=l, dn, ~fill, Token.Unmolded.unmold(tok));
      let (molded, fill, up) =
        if (Rel.get(Terr.face, Slope.face, molded) == tok.mtrl) {
          // fast path for when tok retains original mold
          let extended =
            molded |> Rel.map(Terr.extend(rest), Slope.extend(rest));
          (extended, hd.cell, tl);
        } else {
          let (cell, up) =
            switch (Chain.Affix.split_hd(rest)) {
            | None => (hd.cell, tl)
            | Some((cell, (ts, cs))) =>
              let hd = {...hd, wald: Wald.mk(ts, cs)};
              (cell, [hd, ...tl]);
            };
          (molded, Cell.empty, Slope.(cat(Up.unroll(cell), up)));
        };
      switch (molded) {
      | Eq(l) => Eq((l, fill, Slope.cat(up, Bound.to_list(r))))
      | Neq(dn) => remold(~fill, (dn, up), bound)
      };
    }
  };
