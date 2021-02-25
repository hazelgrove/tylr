open Util;
open OptUtil.Syntax;

module Input:
  Parser.S_INPUT with
    module Tm := Term_exp and module T := Tile_exp and module F := Frame_exp = {
  let sort = (~sort_and_associate) =>
    Tile.get(
      fun
      | Unsorted.Tile.OpHole => Some(Tile.Op(Term_exp.OpHole))
      | Text(s) =>
        if (StringUtil.is_num(s)) {
          Some(Op(Num(int_of_string(s))));
        } else if (StringUtil.is_var(s)) {
          Some(Op(Var(s)));
        } else {
          None;
        }
      | Paren(body) => {
          let+ body = sort_and_associate(body);
          Tile.Op(Term_exp.Paren(body));
        },
      fun
      | Unsorted.Tile.Lam(p) => {
          let+ p = Parser_pat.sort_and_associate(p);
          Tile.Pre(Term_exp.Lam(p));
        }
      | Let(p, def) => {
          let+ p = Parser_pat.sort_and_associate(p)
          and+ def = sort_and_associate(def);
          Tile.Pre(Term_exp.Let(p, def));
        },
      fun
      | Unsorted.Tile.Ann(_) => None
      | Ap(_) => failwith("ap todo"),
      fun
      | Unsorted.Tile.Arrow => None
      | Plus => Some(Tile.Bin(Term_exp.Plus))
      | BinHole => Some(Bin(BinHole)),
    );

  let unsort = (~dissociate_and_unsort) =>
    Tile.get(
      fun
      | Term_exp.OpHole => Tile.Op(Unsorted.Tile.OpHole)
      | Num(n) => Op(Unsorted.Tile.Text(string_of_int(n)))
      | Var(x) => Op(Unsorted.Tile.Text(x))
      | Paren(body) =>
        Op(Unsorted.Tile.Paren(dissociate_and_unsort(body))),
      fun
      | Term_exp.Lam(p) =>
        Tile.Pre(Unsorted.Tile.Lam(Parser_pat.dissociate_and_unsort(p)))
      | Let(p, def) => {
          let p = Parser_pat.dissociate_and_unsort(p);
          let def = dissociate_and_unsort(def);
          Tile.Pre(Unsorted.Tile.Let(p, def));
        },
      fun
      | Term_exp.Ap(_) => failwith("ap todo"),
      fun
      | Term_exp.BinHole => Tile.Bin(Unsorted.Tile.BinHole)
      | Plus => Bin(Unsorted.Tile.Plus),
    );

  let assemble_tile: AltList.t(Unsorted.Tessera.t, Term_exp.t) => Tile_exp.t =
    fun
    | (Paren_l, [(body, Paren_r)]) => Op(Paren(body))
    // TODO singleton tessera cases?
    | _ => raise(Invalid_argument("Parser_typ.assemble_tile"));

  let disassemble_tile =
    Tile.get(
      fun
      | Term_exp.OpHole => (Unsorted.Tessera.OpHole, [])
      | Num(n) => (Unsorted.Tessera.Text(string_of_int(n)), [])
      | Var(x) => (Unsorted.Tessera.Text(x), [])
      | Paren(body) => (
          Unsorted.Tessera.Paren_l,
          [(body, Unsorted.Tessera.Paren_r)],
        ),
      fun
      | Term_exp.Lam(p) => (
          Unsorted.Tessera.Lam(Parser_pat.dissociate_and_unsort(p)),
          [],
        )
      | Let(p, def) => (
          Let_eq(Parser_pat.dissociate_and_unsort(p)),
          [(def, Let_in)],
        ),
      fun
      | Term_exp.Ap(_) => failwith("ap todo"),
      fun
      | Term_exp.Plus => (Unsorted.Tessera.Plus, [])
      | BinHole => (Unsorted.Tessera.BinHole, []),
    );

  let assemble_open_frame =
      (
        ~associate: list(Tile_exp.t) => Term_exp.t,
        (_prefix, ts, suffix):
          ZZList.t(
            AltList.b_frame(Unsorted.Tessera.t, Term_exp.t),
            Tile_exp.t,
          ),
        frame: Frame_exp.t,
      )
      : Frame_exp.open_ => {
    let e = Invalid_argument("Parser_exp.assemble_open_bidelimited_frame");
    switch (ts) {
    | ((Paren_l, []), (Paren_r, [])) => Paren_body(frame)
    | ((Let_eq(p), []), (Let_in, [])) =>
      let p = OptUtil.get_or_raise(e, Parser_pat.sort_and_associate(p));
      let body = associate(suffix);
      Let_def(p, frame, body);
    | _ => raise(e)
    };
  };

  let disassemble_open_frame = (~dissociate, frame: Frame_exp.open_) =>
    switch (frame) {
    | Paren_body(frame) =>
      let ts = (
        (Unsorted.Tessera.Paren_l, []),
        (Unsorted.Tessera.Paren_r, []),
      );
      (([], ts, []), frame);
    | Let_def(p, frame, body) =>
      let ts = (
        (Unsorted.Tessera.Let_eq(Parser_pat.dissociate_and_unsort(p)), []),
        (Unsorted.Tessera.Let_in, []),
      );
      (([], ts, dissociate(body)), frame);
    | Ap_arg(_) => failwith("ap todo")
    };
};

include Parser.Make(Term_exp, Tile_exp, Frame_exp, Input);
