open Util;
open OptUtil.Syntax;

type elem('tile) =
  | Tile('tile)
  | Tessera(HTessera.t)
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

module Make = (Tile: Tile.S) => {
  type nonrec t = t(Tile.t);

  let parse = (selection: t('tile)): t('tile) => {
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
        let* (tile, selection) = go_open(open_, selection);
        go_open(~rev_tiles=[tile, ...rev_tiles], open_, selection);
      | [Tessera(Close(close)), ...selection] =>
        let ts = List.rev(rev_tiles);
        let+ tile = Tile.mk(open_, ts, close);
        (tile, selection);
      };
    go(selection);
  };

  let is_convex_left =
    fun
    | Tile(tile) => Tile.is_convex_left(tile)
    | Tessera(tessera) => HTessera.is_convex_left(tessera);
  let is_convex_right =
    fun
    | Tile(tile) => Tile.is_convex_right(tile)
    | Tessera(tessera) => HTessera.is_convex_right(tessera);
  let is_hole =
    fun
    | Tile(tile) => Tile.is_hole(tile)
    | Tessera(_) => false;

  let fix_empty_holes_between =
      (prefix: t('tile) as 't, suffix: 't): ('t, 't) =>
    switch (ListUtil.split_last_opt(prefix), suffix) {
    | (None, _)
    | (_, []) => (prefix, suffix)
    | (Some((leading, last)), [first, ...trailing]) =>
      let last_is_convex = is_convex_right(last);
      let first_is_convex = is_convex_left(first);
      if (last_is_convex != first_is_convex) {
        is_hole(last) && is_hole(first)
          ? (leading, trailing) : (prefix, suffix);
      } else if (is_hole(last)) {
        (leading, suffix);
      } else if (is_hole(first)) {
        (prefix, trailing);
      } else {
        let hole =
          Tile(
            last_is_convex ? Bin(Tile.mk_bin_hole()) : Op(Tile.mk_op_hole),
          );
        (prefix, [hole, ...suffix]);
      };
    };

  let rec fix_empty_holes_end = (~side: Direction.t) =>
    fun
    | [] => [] // should never hit this case
    | [[], ...selections] =>
      switch (selections) {
      | [] => [[Op(Tile.mk_op_hole())]]
      | [_, ..._] => [[], ...fix_empty_holes_end(selections)]
      }
    | [[elem, ...selection'] as selection, ...selections] =>
      if (!is_convex_left(elem) && is_hole(elem)) {
        [
          selection',
          ...switch (selection') {
             | [] => fix_empty_holes_left(selections)
             | [_, ..._] => selections
             },
        ];
      } else {
        let cap =
          switch (t) {
          | Op(_)
          | Pre(_) => []
          | Post(_)
          | Bin(_) => [Op(Tile.mk_op_hole())]
          };
        switch (side) {
        | Left => [cap @ selection, ...selections]
        | Right => [cap, selection, ...selections]
        };
      };

  let fix_empty_holes_left = fix_empty_holes_end(~side=Left);
  let fix_empty_holes_right = selections =>
    selections
    |> List.rev_map(Prototile.rev)
    |> fix_empty_holes_end(~side=Right)
    |> List.rev_map(Prototile.rev);

  let fix_empty_holes = (selections: list(t)): list(t) => {
    let rec fix = (selection: t, selections: list(t)): list(t) => {
      let skip_empty = (selection, selections) => {
        let (selection, selections) =
          ListUtil.split_first(fix(selection, selections));
        [selection, [], ...selections];
      };
      switch (selections) {
      | [] => [selection]
      | [[], ...selections] => skip_empty(selection, selections)
      | [[_, ..._] as selection', ...selections] =>
        let (selection, selection') =
          fix_empty_holes_between(selection, selection');
        switch (selection') {
        | [] => skip_empty(selection, selections)
        | [_, ..._] => [selection, selection', ...selections]
        };
      };
    };
    let fixed_between = List.fold_right(fix, selections, []);
    fix_empty_holes_left(fix_empty_holes_right(fixed_between));
  };
};
