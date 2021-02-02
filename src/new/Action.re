open Sexplib.Std;

[@deriving sexp]
type t =
  | Mark
  | Move(Direction.t)
  | Delete(Direction.t)
  | Construct(HTile.shape);

let perform = (a: t, zipper: Zipper.t): option(Zipper.t) =>
  switch (a) {
  | Mark => Zipper.mark(zipper)
  | Move(d) => Zipper.move(d, zipper)
  | Delete(d) => Zipper.delete(d, zipper)
  | Construct(shape) => Zipper.construct(shape, zipper)
  };
