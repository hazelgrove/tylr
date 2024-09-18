[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  cur: Cursor.t(LCell.t, LZigg.t),
  ctx: LFrame.t,
};

let mk = (cur, ctx) => {cur, ctx};
