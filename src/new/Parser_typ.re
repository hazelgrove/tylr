open Util;
open OptUtil.Syntax;

module Input:
  Parser.S_INPUT with module Tm := Term_typ and module T := Tile_typ with
    module F := Frame_typ = {
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

  let assemble_tile: AltList.t(Unsorted.Tessera.t, Term_typ.t) => Tile_typ.t =
    fun
    | (Paren_l, [(body, Paren_r)]) => Op(Paren(body))
    // TODO singleton tessera cases?
    | _ => raise(Invalid_argument("Parser_typ.assemble_tile"));

  let disassemble_tile =
    Tile.get(
      fun
      | Term_typ.OpHole => (Unsorted.Tessera.OpHole, [])
      | Num => (Unsorted.Tessera.Text("Num"), [])
      | Bool => (Unsorted.Tessera.Text("Bool"), [])
      | Paren(body) => (Paren_l, [(body, Unsorted.Tessera.Paren_r)]),
      () => raise(Term_typ.Void_pre),
      () => raise(Term_typ.Void_post),
      fun
      | Term_typ.BinHole => (Unsorted.Tessera.BinHole, [])
      | Arrow => (Unsorted.Tessera.Arrow, []),
    );

  let assemble_open_bidelimited_frame =
      (
        ~associate as _,
        (_prefix, ts, _suffix):
          ZZList.t(
            AltList.b_frame(Unsorted.Tessera.t, Term_typ.t),
            Tile_typ.t,
          ),
        frame: Frame_typ.t,
      )
      : Frame_typ.bidelimited => {
    switch (ts) {
    | ((Paren_l, []), (Paren_r, [])) => Paren_body(frame)
    | _ =>
      raise(Invalid_argument("Parser_typ.assemble_open_bidelimited_frame"))
    };
  };
};

include Parser.Make(Term_typ, Tile_typ, Frame_typ, Input);
