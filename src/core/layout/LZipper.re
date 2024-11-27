[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  cur: Cursor.t(LCell.t, LZigg.t),
  ctx: LCtx.t,
  eqs: (LEqs.t, LEqs.t),
};

let mk = (~eqs=([], []), cur, ctx) => {cur, ctx, eqs};
