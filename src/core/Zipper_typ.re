type t = (Subject.t(Tile_typ.t), Frame_typ.bidelimited);
type pointing = (Subject.pointing(Tile_typ.t), Frame_typ.bidelimited);
type selecting = (Subject.selecting(Tile_typ.t), Frame_typ.bidelimited);
type restructuring = (
  Subject.restructuring(Tile_typ.t),
  Frame_typ.bidelimited,
);

type subterm = (Term_typ.t, Frame_typ.t);
