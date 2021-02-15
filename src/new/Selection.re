open Util;

type elem('tile) =
  | Tile('tile)
  | Tessera(Unsorted.Tessera.t)
constraint 'tile = Tile.t('op, 'pre, 'post, 'bin);
type t('tile) = list(elem('tile));

exception Invalid_selection;

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
