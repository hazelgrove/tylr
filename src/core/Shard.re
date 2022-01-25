open Sexplib.Std;

module Index = {
  [@deriving sexp]
  type t = int;
};

module Form = {
  [@deriving sexp]
  type t = (Index.t, Tile.Form.t);

  let consistent = (matching: list((Index.t, Nibs.t)), mold: Tile.Mold.t) =>
    matching
    |> List.for_all(((index, nibs)) => Tile.Mold.nibs(~index, mold) == nibs);

  let nibs =
      (~matching: list((Index.t, Nibs.t))=[], (index, form)): list(Nibs.t) =>
    Tile.Form.molds(form)
    |> List.filter(consistent(matching))
    |> List.map(Tile.Mold.nibs(~index));
};

[@deriving sexp]
type t = {
  tile_id: Tile.Id.t,
  form: Form.t,
  nibs: Nibs.t,
};
