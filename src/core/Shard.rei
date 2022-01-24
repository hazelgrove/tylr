module Index: {type t = int;};

module Form: {
  type t = (Index.t, Tile.Form.t);

  let nibs: (~matching: list((Index.t, Nibs.t))=?, t) => list(Nibs.t);
};

type t = {
  tile_id: Tile.Id.t,
  form: Form.t,
  nibs: Nibs.t,
};
