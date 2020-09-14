open Virtual_dom.Vdom;

let rec length = (ty: HTyp.t) =>
  Code.length(
    length_of_operand,
    length_of_preop,
    length_of_postop,
    length_of_binop,
    ty,
  )
and length_of_operand =
  fun
  | OperandHole => 1
  | Num => 3
  | Paren(body) => 4 + length(body)
and length_of_preop =
  fun
  | _ => raise(HTyp.Tile.Void_PreOp)
and length_of_postop =
  fun
  | _ => raise(HTyp.Tile.Void_PostOp)
and length_of_binop =
  fun
  | OperatorHole
  | Arrow => 1;

let rec view = (~highlight=false, ty: HTyp.t): Node.t => {
  let root_views =
    switch (HTyp.root(ty)) {
    | Operand(operand) =>
      Code.view_of_Operand(
        ~highlight=highlight ? Some(length_of_operand(operand)) : None,
        view_of_operand(operand),
      )
    | PreOp(_) => raise(HTyp.Tile.Void_PreOp)
    | PostOp(_) => raise(HTyp.Tile.Void_PostOp)
    | BinOp(l, binop, r) =>
      Code.view_of_BinOp(
        ~highlight=highlight ? Some(length_of_binop(binop)) : None,
        view(l),
        view_of_binop(binop),
        view(r),
      )
    };
  Node.span([], root_views);
}
and view_of_operand: HTyp.Tile.operand => list(Node.t) =
  fun
  | OperandHole => Code.view_of_OperandHole
  | Num => [Node.text("Num")]
  | Paren(body) => Code.view_of_Paren(view(body))
and view_of_binop: HTyp.Tile.binop => list(Node.t) =
  fun
  | OperatorHole => Code.view_of_OperatorHole
  | Arrow => Code.view_of_Arrow;

// TODO
let view_of_cursor = (~offset as _, ~constructable as _) =>
  Node.span([], []);

let rec view_z = (zty: ZTyp.t): Node.t =>
  switch (ZTyp.root(zty)) {
  | None =>
    let ZList.{prefix, suffix, _} = zty;
    let cursor = view_of_cursor(~offset=length(prefix), ~constructable=[]);
    let tiles = view(~highlight=true, prefix @ suffix);
    Node.span([], [cursor, tiles]);
  | Some(root) =>
    let root_views =
      switch (root) {
      | OperandZ(zoperand) =>
        Code.view_of_Operand(view_of_zoperand(zoperand))
      | PreOpZ_op(_) => raise(ZTyp.ZTile.Void_ZPreOp)
      | PreOpZ_arg(_) => raise(HTyp.Tile.Void_PreOp)
      | PostOpZ_op(_) => raise(ZTyp.ZTile.Void_ZPostOp)
      | PostOpZ_arg(_) => raise(HTyp.Tile.Void_PostOp)
      | BinOpZ_op(_) => raise(ZTyp.ZTile.Void_ZBinOp)
      | BinOpZ_larg(zl, binop, r) =>
        Code.view_of_BinOp(view_z(zl), view_of_binop(binop), view(r))
      | BinOpZ_rarg(l, binop, zr) =>
        Code.view_of_BinOp(view(l), view_of_binop(binop), view_z(zr))
      };
    Node.span([], root_views);
  }
and view_of_zoperand =
  fun
  | ParenZ_body(zbody) => Code.view_of_Paren(view_z(zbody));
