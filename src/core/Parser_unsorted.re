open Util;
open OptUtil.Syntax;

let disassemble_tile =
  Tile.get(
    fun
    | Unsorted.Tile.OpHole => (Unsorted.Tessera.OpHole, [])
    | Text(s) => (Unsorted.Tessera.Text(s), [])
    | Paren(body) => (
        Unsorted.Tessera.Paren_l,
        [(body, Unsorted.Tessera.Paren_r)],
      ),
    fun
    | Unsorted.Tile.Lam(p) => (Unsorted.Tessera.Lam(p), [])
    | Let(p, def) => (Let_eq(p), [(def, Let_in)]),
    fun
    | Unsorted.Tile.Ann(ann) => (Unsorted.Tessera.Ann(ann), [])
    | Ap(_) => failwith("ap todo"),
    fun
    | Unsorted.Tile.BinHole => (Unsorted.Tessera.BinHole, [])
    | Plus => (Unsorted.Tessera.Plus, [])
    | Arrow => (Unsorted.Tessera.Arrow, []),
  );

let assemble_tile:
  AltList.t(Unsorted.Tessera.t, Unsorted.Tile.s) => option(Unsorted.Tile.t) =
  fun
  | (Paren_l, [(body, Paren_r)]) => Some(Op(Paren(body)))
  | (Let_eq(p), [(def, Let_in)]) => Some(Pre(Let(p, def)))
  | (Text(s), []) => Some(Op(Text(s)))
  | (OpHole, []) => Some(Op(OpHole))
  | (BinHole, []) => Some(Bin(BinHole))
  | (Plus, []) => Some(Bin(Plus))
  | (Arrow, []) => Some(Bin(Arrow))
  | (Lam(p), []) => Some(Pre(Lam(p)))
  | (Ann(ann), []) => Some(Post(Ann(ann)))
  | _ => None;

let assemble_tiles_in_selection' =
    (
      ~assemble_tile:
         AltList.t(Unsorted.Tessera.t, list('tile)) => option('tile),
      selection: Selection.t('tile) as 'selection,
    )
    : 'selection => {
  let rec go = (selection: 'selection): 'selection =>
    switch (selection) {
    | [] => []
    | [Tile(_) as elem, ...selection] => [elem, ...go(selection)]
    | [Tessera(tessera) as elem, ...selection'] =>
      if (Unsorted.Tessera.is_closing(tessera)) {
        [elem, ...go(selection')];
      } else {
        // TODO handle mid
        switch (go_opening(tessera, selection')) {
        | None => selection
        | Some((tile, selection)) => [Tile(tile), ...go(selection)]
        };
      }
    }
  and go_opening =
      (
        ~rev_tiles: list('tile)=[],
        open_: Unsorted.Tessera.t,
        selection: 'selection,
      )
      : option(('tile, 'selection)) =>
    switch (selection) {
    | [] => None
    | [Tile(tile), ...selection] =>
      go_opening(~rev_tiles=[tile, ...rev_tiles], open_, selection)
    | [Tessera(tessera), ...selection] =>
      // TODO handle mid tesserae
      if (Unsorted.Tessera.is_closing(tessera)) {
        let ts = List.rev(rev_tiles);
        let+ tile = assemble_tile((open_, [(ts, tessera)]));
        (tile, selection);
      } else {
        let* (tile, selection) = go_opening(open_, selection);
        go_opening(~rev_tiles=[tile, ...rev_tiles], open_, selection);
      }
    };
  go(selection);
};
let assemble_tiles_in_selection =
  assemble_tiles_in_selection'(~assemble_tile);
