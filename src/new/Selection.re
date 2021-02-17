open Util;

type elem('tile) =
  | Tile('tile)
  | Tessera(Unsorted.Tessera.t)
constraint 'tile = Tile.t(_);
type t('tile) = list(elem('tile));

let tile = t => Tile(t);
let tessera = t => Tessera(t);

let map_tile = f =>
  List.map(
    fun
    | Tessera(t) => Tessera(t)
    | Tile(tile) => Tile(f(tile)),
  );

let get_tessera =
  fun
  | Tessera(tessera) => tessera
  | Tile(_) => raise(Invalid_argument("Selection.get_tessera"));

let is_whole = (selection: t('tile)): option(list('tile)) =>
  selection
  |> List.map(
       fun
       | Tile(tile) => Some(tile)
       | Tessera(_) => None,
     )
  |> OptUtil.sequence;

let get_whole = selection =>
  OptUtil.get_or_fail(
    "Selection.get_whole: expected selection to be whole",
    is_whole(selection),
  );
