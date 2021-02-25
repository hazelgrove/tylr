open Util;
open OptUtil.Syntax;

module Input:
  Parser.S_INPUT with
    module Tm := Term_pat and module T := Tile_pat and module F := Frame_pat = {
  let sort = (~sort_and_associate) =>
    Tile.get(
      fun
      | Unsorted.Tile.OpHole => Some(Tile.Op(Term_pat.OpHole))
      | Text(s) =>
        if (StringUtil.is_var(s)) {
          Some(Op(Term_pat.Var(s)));
        } else {
          None;
        }
      | Paren(body) => {
          let+ body = sort_and_associate(body);
          Tile.Op(Term_pat.Paren(body));
        },
      fun
      | Unsorted.Tile.Lam(_)
      | Let(_) => None,
      fun
      | Unsorted.Tile.Ap(_) => None
      | Ann(ann) => {
          let+ ann = Parser_typ.sort_and_associate(ann);
          Tile.Post(Term_pat.Ann(ann));
        },
      fun
      | Unsorted.Tile.Arrow
      | Plus => None
      | BinHole => Some(Tile.Bin(Term_pat.BinHole)),
    );

  let unsort = (~dissociate_and_unsort) =>
    Tile.get(
      fun
      | Term_pat.OpHole => Tile.Op(Unsorted.Tile.OpHole)
      | Var(x) => Op(Unsorted.Tile.Text(x))
      | Paren(body) =>
        Op(Unsorted.Tile.Paren(dissociate_and_unsort(body))),
      () => raise(Term_pat.Void_pre),
      fun
      | Term_pat.Ann(ann) =>
        Tile.Post(Unsorted.Tile.Ann(Parser_typ.dissociate_and_unsort(ann))),
      fun
      | Term_pat.BinHole => Tile.Bin(Unsorted.Tile.BinHole),
    );

  let assemble_tile: AltList.t(Unsorted.Tessera.t, Term_pat.t) => Tile_pat.t =
    fun
    | (Paren_l, [(body, Paren_r)]) => Op(Paren(body))
    // TODO singleton tessera cases?
    | _ => raise(Invalid_argument("Parser_typ.assemble_tile"));

  let disassemble_tile =
    Tile.get(
      fun
      | Term_pat.OpHole => (Unsorted.Tessera.OpHole, [])
      | Var(x) => (Unsorted.Tessera.Text(x), [])
      | Paren(body) => (Paren_l, [(body, Unsorted.Tessera.Paren_r)]),
      () => raise(Term_pat.Void_pre),
      fun
      | Term_pat.Ann(ann) => (
          Unsorted.Tessera.Ann(Parser_typ.dissociate_and_unsort(ann)),
          [],
        ),
      fun
      | Term_pat.BinHole => (Unsorted.Tessera.BinHole, []),
    );

  let assemble_open_frame =
      (
        ~associate as _,
        (_prefix, ts, _suffix):
          ZZList.t(
            AltList.b_frame(Unsorted.Tessera.t, Term_pat.t),
            Tile_pat.t,
          ),
        frame: Frame_pat.t,
      )
      : Frame_pat.open_ => {
    switch (ts) {
    | ((Paren_l, []), (Paren_r, [])) => Paren_body(frame)
    | _ => raise(Invalid_argument("Parser_exp.assemble_open_frame"))
    };
  };

  let disassemble_open_frame = (~dissociate as _, frame: Frame_pat.open_) =>
    switch (frame) {
    | Paren_body(frame) =>
      let ts = (
        (Unsorted.Tessera.Paren_l, []),
        (Unsorted.Tessera.Paren_r, []),
      );
      (([], ts, []), frame);
    };
};

include Parser.Make(Term_pat, Tile_pat, Frame_pat, Input);
