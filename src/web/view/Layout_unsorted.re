open Layout;

let shape_of_tile: Unsorted.Tile.t => Decoration.Tile.shape =
  fun
  | Op(OpHole) => Op(true)
  | Op(_) => Op(false)
  | Pre(_) => Pre()
  | Post(_) => Post()
  | Bin(BinHole) => Bin(true)
  | Bin(_) => Bin(false);

let rec mk_tiles = (tiles: Unsorted.Tile.s) =>
  grouts(List.map(mk_tile, tiles))
and mk_tile = (~style=?, tile: Unsorted.Tile.t) => {
  let l =
    tile
    |> Tile.get(
         fun
         | OpHole => mk_OpHole()
         | Text(s) => Text(s)
         | Paren(body) => mk_Paren(mk_tiles(body)),
         fun
         | Lam(p) => mk_Lam(mk_tiles(p))
         | Let(p, def) => mk_Let(mk_tiles(p), mk_tiles(def)),
         fun
         | Ap(_) => failwith("ap todo")
         | Ann(ann) => mk_Ann(mk_tiles(ann)),
         fun
         | BinHole => mk_BinHole()
         | Plus => mk_Plus()
         | Arrow => mk_Arrow(),
       );
  switch (style) {
  | None => l
  | Some(style) => Annot(Tile(shape_of_tile(tile), style), l)
  };
};

let shape_of_tessera: Unsorted.Tessera.t => Decoration.Tessera.shape =
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
  | Arrow => Bin(false)
  | Let_in => Bin(true);

let mk_tessera = (style, tessera: Unsorted.Tessera.t): t => {
  let l =
    switch (tessera) {
    | OpHole => mk_OpHole()
    | BinHole => mk_BinHole()
    | Text(s) => Text(s)
    | Lam(p) => mk_Lam(mk_tiles(p))
    | Ann(ann) => mk_Ann(mk_tiles(ann))
    | Plus => mk_Plus()
    | Arrow => mk_Arrow()
    | Paren_l => delim("(")
    | Paren_r => delim(")")
    | Let_eq(p) => cats([delim("let"), mk_tiles(p), delim("=")])
    | Let_in => delim("in")
    };
  Annot(Tessera(shape_of_tessera(tessera), style), l);
};
