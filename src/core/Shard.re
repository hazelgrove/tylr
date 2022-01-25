open Sexplib.Std;

module Index = {
  [@deriving sexp]
  type t = int;
  let compare = Int.compare;
};

module Form = {
  [@deriving sexp]
  type t = (Index.t, Tile.Form.t);

  let consistent = (matching: list((Index.t, Nibs.t)), mold: Tile.Mold.t) =>
    matching
    |> List.for_all(((index, nibs)) => Tile.Mold.nibs(~index, mold) == nibs);

  let nibs =
      (
        ~l as _: option(Nib.t)=?,
        matching: list((Index.t, Nibs.t)),
        (index, form),
      )
      : list(Nibs.t) =>
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

module Map = {
  include Map.Make(Index);
};

module Ctx = {
  type shard = t;
  type t = Tile.Map.t(Map.t(Nibs.t));

  let lookup = (_, _) => failwith("todo Shard.Ctx.lookup");

  let add = (shard: shard, ctx: t) => {
    let shard_map =
      ctx |> lookup(shard.tile_id) |> Map.add(fst(shard.form), shard.nibs);
    Tile.Map.add(shard.tile_id, shard_map, ctx);
  };
};

let nibs = (~l: option(Nib.t)=?, ctx: Ctx.t, shard: t) =>
  Form.nibs(~l?, Ctx.lookup(shard.tile_id, ctx), shard.form);
