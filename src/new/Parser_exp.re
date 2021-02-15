open Util;
open OptUtil.Syntax;

module Input:
  Parser.S_INPUT with module Term := Term_exp and module Tile := Tile_exp = {
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

  let connect: AltList.t(Unsorted.Tessera.t, Term_exp.t) => Tile_exp.t =
    fun
    | (Paren_l, [(body, Paren_r)]) => Op(Paren(body))
    // TODO singleton tessera cases?
    | _ => raise(Invalid_argument("Parser_typ.connect"));

  let disconnect =
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
};

include Parser.Make(Term_exp, Tile_exp, Input);
