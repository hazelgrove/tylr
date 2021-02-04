open Util;
open OptUtil.Syntax;

type elem('tile) =
  | Tile('tile)
  | Tessera(HTessera.t);
type t('tile) = list(elem('tile));

exception Invalid_selection;

// TODO maybe use different term
let is_complete: t(_) => bool =
  List.for_all(
    fun
    | Tile(_) => true
    | Tessera(_) => false,
  );

let parse =
    (
      ~mk_tile:
         (HTessera.open_, list('tile), HTessera.close) => option('tile),
      selection: t('tile),
    )
    : t('tile) => {
  let rec go = (selection: t('tile)): t('tile) =>
    switch (selection) {
    | [] => []
    | [(Tile(_) | Tessera(Close(_))) as elem, ...selection] => [
        elem,
        ...go(selection),
      ]
    | [Tessera(Open(open_)), ...selection'] =>
      switch (go_open(open_, selection')) {
      | None => selection
      | Some((tile, selection)) => [Tile(tile), ...go(selection)]
      }
    }
  and go_open =
      (
        ~rev_tiles: list('tile)=[],
        open_: HTessera.open_,
        selection: t('tile),
      )
      : option(('tile, t('tile))) =>
    switch (selection) {
    | [] => None
    | [Tile(tile), ...selection] =>
      go_open(~rev_tiles=[tile, ...rev_tiles], open_, selection)
    | [Tessera(Open(open_)), ...selection] =>
      let+ (tile, selection) = go_open(open_, selection);
      go_open(~rev_tiles=[tile, ...rev_tiles], open_, selection);
    | [Tessera(Close(close)), ...selection] =>
      let ts = List.rev(rev_tiles);
      let+ tile = mk_tile(open_, ts, close);
      (tile, selection);
    };
  go(selection);
};
