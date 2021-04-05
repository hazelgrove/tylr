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
    | Plus => (Plus, [])
    | Arrow => (Arrow, [])
    | Prod => (Prod, [])
    | Cond(then_) => (Cond_then, [(then_, Cond_else)]),
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

let rec find_rest_of_tile =
        (
          d: Direction.t,
          tessera: Unsorted.Tessera.t as 't,
          selection: Selection.t('tile) as 's,
        )
        : option((AltList.even(list('tile), 't), 's)) =>
  if (Unsorted.Tessera.is_end_of_tile(d, tessera)) {
    Some(([], selection));
  } else {
    switch (selection) {
    | [] => None
    | [Tessera(next), ...selection] =>
      if (Unsorted.Tessera.is_next(d, tessera, next)) {
        let+ (rest, remaining) = find_rest_of_tile(d, next, selection);
        ([([], next), ...rest], remaining);
      } else {
        None;
      }
    | [Tile(tile), ...selection] =>
      let+ (rest, remaining) = find_rest_of_tile(d, tessera, selection);
      let ((open_child, tessera), tl) = ListUtil.split_first(rest);
      let open_child =
        switch (d) {
        | Left => open_child @ [tile]
        | Right => [tile, ...open_child]
        };
      ([(open_child, tessera), ...tl], remaining);
    };
  };

let rec assemble_tiles_in_selection' =
        (
          ~direction: Direction.t,
          ~assemble_tile:
             AltList.t(Unsorted.Tessera.t, list('tile)) => option('tile),
          selection: Selection.t('tile) as 'selection,
        )
        : 'selection => {
  let go = assemble_tiles_in_selection'(~direction, ~assemble_tile);
  switch (selection) {
  | [] => []
  | [Tile(_) as elem, ...selection] => [elem, ...go(selection)]
  | [Tessera(tessera) as elem, ...selection'] =>
    let cannot_assemble = () => [elem, ...go(selection')];
    switch (find_rest_of_tile(direction, tessera, selection')) {
    | None => cannot_assemble()
    | Some((rest, selection')) =>
      let disassembled =
        switch (direction) {
        | Left => AltList.rev((tessera, rest))
        | Right => (tessera, rest)
        };
      switch (assemble_tile(disassembled)) {
      | None => cannot_assemble()
      | Some(tile) => [Tile(tile), ...go(selection')]
      };
    };
  };
};
let assemble_tiles_in_selection =
  assemble_tiles_in_selection'(~assemble_tile);
