[@deriving sexp]
type t = (Subject.t(Tile_typ.t), Frame_typ.bidelimited);
[@deriving sexp]
type pointing = (Subject.pointing(Tile_typ.t), Frame_typ.bidelimited);
[@deriving sexp]
type selecting = (Subject.selecting(Tile_typ.t), Frame_typ.bidelimited);
[@deriving sexp]
type restructuring = (
  Subject.restructuring(Tile_typ.t),
  Frame_typ.bidelimited,
);

[@deriving sexp]
type subterm = (Term_typ.t, Frame_typ.t);
