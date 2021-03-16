type t = (Subject.t(Tile_exp.t), Frame_exp.bidelimited);
type pointing = (Subject.pointing(Tile_exp.t), Frame_exp.bidelimited);
type selecting = (Subject.selecting(Tile_exp.t), Frame_exp.bidelimited);
type restructuring = (
  Subject.restructuring(Tile_exp.t),
  Frame_exp.bidelimited,
);

type subterm = (Term_exp.t, Frame_exp.t);
