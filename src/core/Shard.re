module Index = {
  type t = int;
};

module Form = {
  type t = (Index.t, Tile.Form.t);

  let consistent = (matching: list((Index.t, Nibs.t)), mold: Mold.t) =>
    matching
    |> List.for_all(((index, nibs)) => Mold.nibs(~index, mold) == nibs);

  let nibs =
      (~matching: list((Index.t, Nibs.t))=[], (index, form)): list(Nibs.t) =>
    Tile.Forms.molds(form)
    |> List.filter(consistent(matching))
    |> List.map(Mold.nibs(~index));
};

type t = {
  tile_id: Tile.Id.t,
  form: Form.t,
  nibs: Nibs.t,
};
