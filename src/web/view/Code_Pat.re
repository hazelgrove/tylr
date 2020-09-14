open Virtual_dom.Vdom;

let rec length = (p: HPat.t) =>
  Code.length(
    length_of_operand,
    length_of_preop,
    length_of_postop,
    length_of_binop,
    p,
  )
and length_of_operand: HPat.Tile.operand => int =
  fun
  | OperandHole => 1
  | Var(x) => String.length(x)
  | Paren(body) => 2 + length(body) + 2
and length_of_preop: HPat.Tile.preop => int =
  fun
  | _ => raise(HPat.Tile.Void_PreOp)
and length_of_postop: HPat.Tile.postop => int =
  fun
  | Ann(_, ann) => 2 + Code_Typ.length(ann)
and length_of_binop: HPat.Tile.binop => int =
  fun
  | OperatorHole => 1;

// TODO
let view_of_status = _ => Node.span([], []);

let rec view = (~highlight=false, p: HPat.t): Node.t => {
  let root_views =
    switch (HPat.root(p)) {
    | Operand(operand) =>
      Code.view_of_Operand(
        ~highlight=highlight ? Some(length_of_operand(operand)) : None,
        view_of_operand(operand),
      )
    | PreOp(_) => raise(HPat.Tile.Void_PreOp)
    | PostOp(arg, postop) =>
      Code.view_of_PostOp(
        ~highlight=highlight ? Some(length_of_postop(postop)) : None,
        view(arg),
        view_of_postop(postop),
      )
    | BinOp(l, binop, r) =>
      Code.view_of_BinOp(
        ~highlight=highlight ? Some(length_of_binop(binop)) : None,
        view(l),
        view_of_binop(binop),
        view(r),
      )
    };
  let status_view = view_of_status(p);
  Node.span([], [status_view, ...root_views]);
}
and view_of_operand: HPat.Tile.operand => list(Node.t) =
  fun
  | OperandHole => Code.view_of_OperandHole
  | Var(x) => Code.view_of_Var(x)
  | Paren(body) => Code.view_of_Paren(view(body))
and view_of_postop: HPat.Tile.postop => list(Node.t) =
  fun
  | Ann(_, ann) => Code.view_of_Ann(Code_Typ.view(ann))
and view_of_binop: HPat.Tile.binop => list(Node.t) =
  fun
  | OperatorHole => Code.view_of_OperatorHole;

// TODO
let view_of_cursor = (~offset as _, ~constructable as _) =>
  Node.span([], []);

let rec view_z = (zp: ZPat.t) =>
  switch (ZPat.root(zp)) {
  | None =>
    let ZList.{prefix, suffix, _} = zp;
    let cursor = view_of_cursor(~offset=length(prefix), ~constructable=[]);
    let tiles = view(~highlight=true, prefix @ suffix);
    Node.span([], [cursor, tiles]);
  | Some(root) =>
    let root_views =
      switch (root) {
      | OperandZ(zoperand) =>
        Code.view_of_Operand(view_of_zoperand(zoperand))
      | PreOpZ_op(_) => raise(ZPat.ZTile.Void_ZPreOp)
      | PreOpZ_arg(_) => raise(HPat.Tile.Void_PreOp)
      | PostOpZ_op(arg, zpostop) =>
        Code.view_of_PostOp(view(arg), view_of_zpostop(zpostop))
      | PostOpZ_arg(zarg, postop) =>
        Code.view_of_PostOp(view_z(zarg), view_of_postop(postop))
      | BinOpZ_op(_) => raise(ZPat.ZTile.Void_ZBinOp)
      | BinOpZ_larg(zl, binop, r) =>
        Code.view_of_BinOp(view_z(zl), view_of_binop(binop), view(r))
      | BinOpZ_rarg(l, binop, zr) =>
        Code.view_of_BinOp(view(l), view_of_binop(binop), view_z(zr))
      };
    let status_view = view_of_status(ZPat.erase(zp));
    Node.span([], [status_view, ...root_views]);
  }
and view_of_zoperand =
  fun
  | ParenZ_body(zbody) => Code.view_of_Paren(view_z(zbody))
and view_of_zpostop =
  fun
  | AnnZ_ann(_, zann) => Code.view_of_Ann(Code_Typ.view_z(zann));
