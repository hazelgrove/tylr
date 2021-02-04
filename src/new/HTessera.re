// TODO rename to avoid clash with open/closed children
type open_ =
  | Paren_l
  | Let_eq(/* pattern */ HTile.s);

type close =
  | Paren_r
  | Let_in;

// TODO add Mid
type t =
  | Open(open_)
  | Close(close);

// TODO review and consider making
// HTile and HTessera mutually recursive
let mk_tile = (open_: open_, ts: HTile.s, close: close): option(HTile.t) =>
  switch (open_, close) {
  | (Paren_l, Paren_r) => Some(Op(Paren(ts)))
  | (Let_eq(p), Let_in) => Some(Pre(Let(p, ts)))
  | _ => None
  };
