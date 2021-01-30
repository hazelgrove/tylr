type elem =
  | Tile(HTile.t)
  | Tessera(HTessera.t);
type t = list(elem);

exception Invalid_selection;

let is_complete: t => bool =
  List.for_all(
    fun
    | Tile(_) => true
    | Tessera(_) => false,
  );

let parse = (selection: t): t => {
  let rec go = (selection: t): t =>
    switch (selection) {
    | [] => []
    | [Tile(_) | Tessera(Close(_)) as elem, ...selection] => [
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
      (~rev_tiles: HTile.s=[], open_: HTessera.open_, selection: t)
      : option((HTile.t, t)) =>
    switch (selection) {
    | [] => None
    | [Tile(tile), ...selection] =>
      go_open(~rev_tiles=[tile, ...rev_tiles], open_, selection)
    | [Tessera(Open(open_)), ...selection] =>
      let+ (tile, selection) = go_open(open_, selection);
      go_open(~rev_tiles=[tile, ...rev_tiles], open_, selection);
    | [Tessera(Close(_)), ...selection] =>
      let ts = List.rev(rev_tiles);
      let tile =
        switch (open_) {
        | Paren_l => HTile.Paren(ts)
        | Let_eq(p) => Let(p, ts)
        };
      Some((tile, selection));
    };
  go(selection);
};
