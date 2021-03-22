[@deriving sexp]
type t = (Subject.t(Tile_pat.t), Frame_pat.bidelimited);
[@deriving sexp]
type pointing = (Subject.pointing(Tile_pat.t), Frame_pat.bidelimited);
[@deriving sexp]
type selecting = (Subject.selecting(Tile_pat.t), Frame_pat.bidelimited);
[@deriving sexp]
type restructuring = (
  Subject.restructuring(Tile_pat.t),
  Frame_pat.bidelimited,
);

[@deriving sexp]
type subterm = (Term_pat.t, Frame_pat.t);
