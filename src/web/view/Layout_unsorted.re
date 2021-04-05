open Core;
open Layout;

let shape_of_tile: Unsorted.Tile.t => Layout.tile_shape =
  fun
  | Op(OpHole) => Op(true)
  | Op(_) => Op(false)
  | Pre(_) => Pre()
  | Post(_) => Post()
  | Bin(BinHole) => Bin(true)
  | Bin(_) => Bin(false);

// HACK sort arg present because I'm happening to
// reuse this function for sorted tiles that are not
// yet selected
let rec mk_tiles = (~sort=?, tiles: Unsorted.Tile.s) =>
  grouts(List.map(mk_tile(~style=?None, ~sort?), tiles))
and mk_tile = (~style=?, ~sort=?, tile: Unsorted.Tile.t) => {
  let with_sort = (s: Sort.t) => Option.map(_ => s, sort);
  // TODO I don't think I need to tag open/closed children here
  let (l, _) =
    tile
    |> Tile.get(
         fun
         | Unsorted.Tile.OpHole => (
             empty_hole(~sort?, fst(mk_OpHole())),
             None,
           )
         | Text(s) => mk_text(s)
         | Paren(body) => mk_Paren(open_child(mk_tiles(~sort?, body))),
         fun
         | Unsorted.Tile.Lam(p) =>
           mk_Lam(closed_child(mk_tiles(~sort=?with_sort(Pat), p)))
         | Let(p, def) =>
           mk_Let(
             closed_child(mk_tiles(~sort=?with_sort(Pat), p)),
             open_child(mk_tiles(~sort?, def)),
           ),
         fun
         | Unsorted.Tile.Ap(_) => failwith("ap todo")
         | Ann(ann) => mk_Ann(mk_tiles(~sort=?with_sort(Typ), ann)),
         fun
         | Unsorted.Tile.BinHole => (
             empty_hole(~sort?, fst(mk_BinHole())),
             None,
           )
         | Plus => mk_Plus()
         | Arrow => mk_Arrow()
         | Prod => mk_Prod()
         | Cond(then_) => mk_Cond(open_child(mk_tiles(~sort?, then_))),
       );
  switch (style) {
  | None => l
  | Some(style) => Annot(Tile(shape_of_tile(tile), style), l)
  };
};

let shape_of_tessera: Unsorted.Tessera.t => Layout.tessera_shape =
  fun
  | OpHole
  | Text(_) => Op()
  | Lam(_) => Pre(false)
  | Paren_l
  | Let_eq(_) => Pre(true)
  | Ann(_) => Post(false)
  | Paren_r => Post(true)
  | BinHole
  | Plus
  | Arrow
  | Prod => Bin((false, false))
  | Cond_then => Bin((false, true))
  | Cond_else
  | Let_in => Bin((true, false));

let mk_tessera = (~style, tessera: Unsorted.Tessera.t): t => {
  let l =
    switch (tessera) {
    | OpHole => fst(mk_OpHole())
    | BinHole => fst(mk_BinHole())
    | Text(s) => Text(s)
    | Lam(p) => fst(mk_Lam(closed_child(mk_tiles(p))))
    | Ann(ann) => fst(mk_Ann(closed_child(mk_tiles(ann))))
    | Plus => fst(mk_Plus())
    | Arrow => fst(mk_Arrow())
    | Prod => fst(mk_Prod())
    | Paren_l => delim("(")
    | Paren_r => delim(")")
    | Let_eq(p) =>
      cats([delim("let"), closed_child(mk_tiles(p)), delim("=")])
    | Let_in => delim("in")
    | Cond_then => delim("?")
    | Cond_else => delim(":")
    };
  Annot(Tessera(shape_of_tessera(tessera), style), l);
};
