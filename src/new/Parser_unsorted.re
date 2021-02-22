open Util;

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
  AltList.t(Unsorted.Tessera.t, Unsorted.Tile.s) => Unsorted.Tile.t =
  fun
  | (Paren_l, [(body, Paren_r)]) => Op(Paren(body))
  | (Let_eq(p), [(def, Let_in)]) => Pre(Let(p, def))
  | _ => raise(Invalid_argument("Parser_unsorted.assemble_tile"));

let assemble_tiles_in_selection =
  Parser.assemble_tiles_in_selection(~assemble_tile);
