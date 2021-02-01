open Sexplib.Std;

[@deriving sexp]
type tile_shape =
  | Text(string)
  | Paren
  | Lam
  | Let
  | Ap
  | Ann
  | Plus
  | Arrow;

[@deriving sexp]
type t =
  | Mark
  | Move(Direction.t)
  | Delete(Direction.t)
  | Construct(tile_shape);

let perform_normal =
    (a: t, j: EditMode.normal, zipper: Zipper.t): option(EditState.t) =>
  switch (a) {
  | Mark =>
    let selecting = ZList.mk(~z=(j, []), ());
    Some((Selecting(selecting), zipper));

  | Move(d) =>
    let+ (j, zipper) = Zipper.move(d, j, zipper);
    (Normal(j), zipper);
  };

let perform = (a: t, (mode, zipper): EditState.t): option(EditState.t) =>
  switch (mode) {
  | Normal(j) => perform_normal(a, j, zipper)
  | Selecting(selecting) => perform_selecting(a, selecting, zipper)
  | Restructuring(restructuring) =>
    perform_restructuring(a, restructuring, zipper)
  };
