// TODO may need to extend with OpHole/BinHole
type singleton =
  | Text(string)
  | Lam(HTile.s)
  | Ann(HTile.s)
  | Plus
  | Arrow;

// TODO rename to avoid clash with open/closed children
type open_ =
  | Paren_l
  | Let_eq(/* pattern */ HTile.s);

type close =
  | Paren_r
  | Let_in;

// TODO add Mid
type t =
  | Singleton(singleton)
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

module Shape = {
  type t =
    | Text(string)
    | Paren_l
    | Paren_r
    | Lam
    | Let_eq
    | Let_in
    | Ann
    | Plus
    | Arrow;
};

let of_shape: Shape.t => t =
  fun
  | Text(s) => Singleton(Text(s))
  | Paren_l => Open(Paren_l)
  | Paren_r => Close(Paren_r)
  | Lam => Singleton(Lam([Op(OpHole)]))
  | Let_eq => Open(Let_eq([Op(OpHole)]))
  | Let_in => Close(Let_in)
  | Ann => Singleton(Ann([Op(OpHole)]))
  | Plus => Singleton(Plus)
  | Arrow => Singleton(Arrow);
