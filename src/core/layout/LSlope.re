include Slope.Base;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Slope.Base.t(Block.t);

let rec roll = (~onto: Dir.t, ~fill=LCell.empty, slope: t) =>
  switch (slope) {
  | [] => fill
  | [hd, ...tl] =>
    let m =
      switch (onto) {
      | L => Meld.mk(~l=hd.cell, Wald.rev(hd.wald), ~r=fill)
      | R => Meld.mk(~l=fill, hd.wald, ~r=hd.cell)
      };
    roll(~onto, ~fill=LCell.wrap(m), tl);
  };
let unroll = (~from: Dir.t, cell: LCell.t) => {
  let rec go = (cell: LCell.t, unrolled) =>
    switch (cell.meld) {
    | None => unrolled
    | Some(M(l, w, r)) =>
      let (cell, terr) =
        switch (from) {
        | L => (r, Terr.Base.{wald: LWald.rev(w), cell: l})
        | R => (l, Terr.Base.{wald: w, cell: r})
        };
      go(cell, [terr, ...unrolled]);
    };
  go(cell, []);
};

module Dn = {
  let roll = roll(~onto=L);
  let unroll = unroll(~from=L);
  let flatten = dn => dn |> List.rev_map(LTerr.R.flatten) |> Block.hcats;
};
module Up = {
  let roll = roll(~onto=R);
  let unroll = unroll(~from=R);
  let flatten = dn => dn |> List.map(LTerr.L.flatten) |> Block.hcats;
};
