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
        | "num" => Some(Op(Num))
        | "bool" => Some(Op(Bool))
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

  let assemble_open_frame =
      (
        ~associate as _,
        (_prefix, ts, _suffix):
          ZZList.t(
            AltList.b_frame(Unsorted.Tessera.t, Term_typ.t),
            Tile_typ.t,
          ),
        frame: Frame_typ.t,
      )
      : option(Frame_typ.open_) => {
    switch (ts) {
    | ((Paren_l, []), (Paren_r, [])) => Some(Paren_body(frame))
    | _ => None
    };
  };

  let disassemble_open_frame = (~dissociate as _, frame: Frame_typ.open_) =>
    switch (frame) {
    | Paren_body(frame) =>
      let ts = (
        (Unsorted.Tessera.Paren_l, []),
        (Unsorted.Tessera.Paren_r, []),
      );
      (([], ts, []), frame);
    };
};

include Parser.Make(Term_typ, Tile_typ, Frame_typ, Input);
