type t('tile) = list(Either.t('tile, HTessera.t));

exception Invalid_selection;

// TODO maybe use different term
let is_complete: t => bool =
  List.for_all(
    fun
    | L(_tile) => true
    | R(_tessera) => false,
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
    | [L(_tile) | R(Close(_)) as elem, ...selection] => [
        elem,
        ...go(selection),
      ]
    | [R(Open(open_)), ...selection'] =>
      switch (go_open(open_, selection')) {
      | None => selection
      | Some((tile, selection)) => [L(tile), ...go(selection)]
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
    | [L(tile), ...selection] =>
      go_open(~rev_tiles=[tile, ...rev_tiles], open_, selection)
    | [R(Open(open_)), ...selection] =>
      let+ (tile, selection) = go_open(open_, selection);
      go_open(~rev_tiles=[tile, ...rev_tiles], open_, selection);
    | [R(Close(close)), ...selection] =>
      let ts = List.rev(rev_tiles);
      let+ tile = mk_tile(open_, ts, close);
      (tile, selection);
    };
  go(selection);
};
