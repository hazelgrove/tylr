open Util;

type t = (Subject.t, Frame.t);

let zip_up =
    (subject: Selection.t, frame: Frame.t)
    : option((Tile.t, ListFrame.t(Tile.t), Frame.t)) => {
  let tiles = Option.get(Selection.get_tiles(subject));
  let get_pat = () => Option.get(Tiles.get_pat(tiles));
  let get_exp = () => Option.get(Tiles.get_exp(tiles));
  switch (frame) {
  | Pat(Paren_body((tframe, frame))) =>
    let tile = Tile.Pat(Paren(get_pat()));
    let tframe = TupleUtil.map2(Tiles.of_pat, tframe);
    Some((tile, tframe, Pat(frame)));
  | Pat(Lam_pat((tframe, frame))) =>
    let tile = Tile.Exp(Lam(get_pat()));
    let tframe = TupleUtil.map2(Tiles.of_exp, tframe);
    Some((tile, tframe, Exp(frame)));
  | Pat(Let_pat(def, (tframe, frame))) =>
    let tile = Tile.Exp(Let(get_pat(), def));
    let tframe = TupleUtil.map2(Tiles.of_exp, tframe);
    Some((tile, tframe, Exp(frame)));
  | Exp(Paren_body((tframe, frame))) =>
    let tile = Tile.Exp(Paren(get_exp()));
    let tframe = TupleUtil.map2(Tiles.of_exp, tframe);
    Some((tile, tframe, Exp(frame)));
  | Exp(Ap_arg((tframe, frame))) =>
    let tile = Tile.Exp(Ap(get_exp()));
    let tframe = TupleUtil.map2(Tiles.of_exp, tframe);
    Some((tile, tframe, Exp(frame)));
  | Exp(Let_def(p, (tframe, frame))) =>
    let tile = Tile.Exp(Let(p, get_exp()));
    let tframe = TupleUtil.map2(Tiles.of_exp, tframe);
    Some((tile, tframe, Exp(frame)));
  | Exp(Cond_then((tframe, frame))) =>
    let tile = Tile.Exp(Cond(get_exp()));
    let tframe = TupleUtil.map2(Tiles.of_exp, tframe);
    Some((tile, tframe, Exp(frame)));
  | Exp(Root) => None
  };
};
