[@deriving (show, sexp)]
type t =
  | Left
  | Right;

let toggle =
  fun
  | Left => Right
  | Right => Left;

let sign =
  fun
  | Left => (-1)
  | Right => 1;

let choose = (d, (l, r)) =>
  switch (d) {
  | Left => l
  | Right => r
  };

[@deriving (show, sexp)]
type plane =
  | L
  | R
  | U
  | D;
