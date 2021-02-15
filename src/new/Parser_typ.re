open Util;
open OptUtil.Syntax;

module Input:
  Parser.S_INPUT with module Term := Term_typ and module Tile := Tile_typ = {
  let sort = (~sort_and_associate) =>
    Tile.get(
      fun
      | Unsorted.Tile.OpHole => Some(Tile.Op(Term_typ.OpHole))
      | Text(s) =>
        switch (s) {
        | "Num" => Some(Op(Num))
        | "Bool" => Some(Op(Bool))
        | _ => None
        }
      | Paren(body) => {
          let+ body = sort_and_associate(body);
          Tile.Op(Term_typ.Paren(body));
        },
      fun
      | Unsorted.Tile.Lam(_)
      | Let(_) => None,
      fun
      | Unsorted.Tile.Ann(_)
      | Ap(_) => None,
      fun
      | Unsorted.Tile.Plus => None
      | Arrow => Some(Tile.Bin(Term_typ.Arrow))
      | BinHole => Some(Bin(BinHole)),
    );

  let unsort = (~dissociate_and_unsort) =>
    Tile.get(
      fun
      | Term_typ.OpHole => Tile.Op(Unsorted.Tile.OpHole)
      | Num => Op(Unsorted.Tile.Text("Num"))
      | Bool => Op(Unsorted.Tile.Text("Bool"))
      | Paren(body) =>
        Op(Unsorted.Tile.Paren(dissociate_and_unsort(body))),
      () => raise(Term_typ.Void_pre),
      () => raise(Term_typ.Void_post),
      fun
      | Term_typ.BinHole => Tile.Bin(Unsorted.Tile.BinHole)
      | Arrow => Tile.Bin(Unsorted.Tile.Arrow),
    );

  let connect: AltList.t(Unsorted.Tessera.t, Term_typ.t) => Tile_typ.t =
    fun
    | (Paren_l, [(body, Paren_r)]) => Op(Paren(body))
    // TODO singleton tessera cases?
    | _ => raise(Invalid_argument("Parser_typ.connect"));

  let disconnect =
    Tile.get(
      fun
      | Term_typ.OpHole => AltList.singleton(Unsorted.Tessera.OpHole)
      | Num => AltList.singleton(Unsorted.Tessera.Text("Num"))
      | Bool => AltList.singleton(Unsorted.Tessera.Text("Bool"))
      | Paren(body) => (Paren_l, [(body, Unsorted.Tessera.Paren_r)]),
      () => raise(Term_typ.Void_pre),
      () => raise(Term_typ.Void_post),
      fun
      | Term_typ.BinHole => AltList.singleton(Unsorted.Tessera.BinHole)
      | Arrow => AltList.singleton(Unsorted.Tessera.Arrow),
    );
};

include Parser.Make(Term_typ, Tile_typ, Input);
