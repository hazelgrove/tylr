open New;
open Layout;

let shape_of_tile: Unsorted.Tile.t => Layout.tile_shape =
  fun
  | Op(OpHole) => Op(true)
  | Op(_) => Op(false)
  | Pre(_) => Pre()
  | Post(_) => Post()
  | Bin(BinHole) => Bin(true)
  | Bin(_) => Bin(false);

let rec mk_tiles = (tiles: Unsorted.Tile.s) =>
  grouts(List.map(mk_tile(~style=?None), tiles))
and mk_tile = (~style=?, tile: Unsorted.Tile.t) => {
  let (l, _) =
    tile
    |> Tile.get(
         fun
         | Unsorted.Tile.OpHole => mk_OpHole()
         | Text(s) => mk_text(s)
         | Paren(body) => mk_Paren(mk_tiles(body)),
         fun
         | Unsorted.Tile.Lam(p) => mk_Lam(mk_tiles(p))
         | Let(p, def) => mk_Let(mk_tiles(p), mk_tiles(def)),
         fun
         | Unsorted.Tile.Ap(_) => failwith("ap todo")
         | Ann(ann) => mk_Ann(mk_tiles(ann)),
         fun
         | Unsorted.Tile.BinHole => mk_BinHole()
         | Plus => mk_Plus()
         | Arrow => mk_Arrow(),
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
  | Arrow => Bin((false, false))
  | Let_in => Bin((true, true));

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
    | Paren_l => delim("(")
    | Paren_r => delim(")")
    | Let_eq(p) =>
      cats([delim("let"), closed_child(mk_tiles(p)), delim("=")])
    | Let_in => delim("in")
    };
  Annot(Tessera(shape_of_tessera(tessera), style), l);
};
