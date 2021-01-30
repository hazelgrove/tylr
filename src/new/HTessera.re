type open_ =
  | Paren_l
  | Let_eq(HTile.s);

type close =
  | Paren_r
  | Let_in;

// TODO add Mid
type t =
  | Open(open_)
  | Close(close);
