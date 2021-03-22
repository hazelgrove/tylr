[@deriving sexp]
type t = (Subject.t(Tile_exp.t), Frame_exp.bidelimited);
[@deriving sexp]
type pointing = (Subject.pointing(Tile_exp.t), Frame_exp.bidelimited);
[@deriving sexp]
type selecting = (Subject.selecting(Tile_exp.t), Frame_exp.bidelimited);
[@deriving sexp]
type restructuring = (
  Subject.restructuring(Tile_exp.t),
  Frame_exp.bidelimited,
);

[@deriving sexp]
type subterm = (Term_exp.t, Frame_exp.t);
