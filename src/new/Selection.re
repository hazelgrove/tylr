module Make = (T: Tile.S) => {
  type elem =
    | Tile(T.t)
    | Tessera(T.tessera);
  type t =
    | ();
};

type elem('tile, 'tessera) =
  | Tile('tile)
  | Tessera('tessera);
type t('tile, 'tessera) = list(elem('tile, 'tessera));
