include Zigg.Base;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Zigg.Base.t(Block.t);

let flatten = (zigg: t) =>
  Block.hcats([
    LSlope.Up.flatten(zigg.up),
    LWald.flatten(~flatten=LCell.flatten, zigg.top),
    LSlope.Dn.flatten(zigg.dn),
  ]);

let roll = (~l, ~r, zigg: t) => {
  let (l, up) = Stds.Lists.split_n(zigg.up, l);
  let (r, dn) = Stds.Lists.split_n(zigg.dn, r);
  (LSlope.Up.roll(l), {...zigg, up, dn}, LSlope.Dn.roll(r));
};

let hd_block = (~side: Dir.t, zigg: t) => {
  let (s_d, _, _) = orient(side, zigg);
  switch (s_d) {
  | [hd, ..._] => Dir.pick(side, LTerr.(L.flatten, R.flatten), hd)
  | [] => LWald.flatten(~flatten=LCell.flatten, zigg.top)
  };
};
