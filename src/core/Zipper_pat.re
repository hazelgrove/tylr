type t = (Subject.t(Tile_pat.t), Frame_pat.bidelimited);
type pointing = (Subject.pointing(Tile_pat.t), Frame_pat.bidelimited);
type selecting = (Subject.selecting(Tile_pat.t), Frame_pat.bidelimited);
type restructuring = (
  Subject.restructuring(Tile_pat.t),
  Frame_pat.bidelimited,
);

type subterm = (Term_pat.t, Frame_pat.t);
