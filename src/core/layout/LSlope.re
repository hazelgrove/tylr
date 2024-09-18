include Slope.Base;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Slope.Base.t(Block.t);

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
  let unroll = unroll(~from=L);
};
module Up = {
  let unroll = unroll(~from=R);
};
