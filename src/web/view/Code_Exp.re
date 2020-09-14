open Virtual_dom.Vdom;

let rec length = (e: HExp.t) =>
  Code.length(
    length_of_operand,
    length_of_preop,
    length_of_postop,
    length_of_binop,
    e,
  )
and length_of_operand =
  fun
  | OperandHole => 1
  | Var(_, x) => String.length(x)
  | Num(_, n) => String.length(string_of_int(n))
  | Paren(body) => 4 + length(body)
and length_of_preop =
  fun
  | Lam(_, p) => 4 + Code_Pat.length(p)
and length_of_postop =
  fun
  | Ap(_, arg) => 4 + length(arg)
and length_of_binop =
  fun
  | OperatorHole
  | Plus(_) => 1;

// TODO
let view_of_status = (_: HExp.t): Node.t => {
  Node.span([], []);
};

let rec view = (~highlight=false, e: HExp.t): Node.t => {
  let root_views =
    switch (HExp.root(e)) {
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
  let status_view = view_of_status(e);
  Node.span([], [status_view, ...root_views]);
}
and view_of_operand =
  fun
  | OperandHole => Code.view_of_OperandHole
  | Var(_, x) => Code.view_of_Var(x)
  | Num(_, n) => [Node.text(string_of_int(n))]
  | Paren(body) => Code.view_of_Paren(view(body))
and view_of_preop: HExp.Tile.preop => list(Node.t) =
  fun
  | Lam(_, p) => Code.view_of_Lam(Code_Pat.view(p))
and view_of_postop =
  fun
  | Ap(_, arg) => Code.view_of_Ap(view(arg))
and view_of_binop =
  fun
  | Plus(_) => Code.view_of_Plus
  | OperatorHole => Code.view_of_OperatorHole;

// TODO
let view_of_cursor = (~offset as _, ~constructable as _) =>
  Node.span([], []);

let rec view_z = (ze: ZExp.t) =>
  switch (ZExp.root(ze)) {
  | None =>
    let ZList.{prefix, suffix, _} = ze;
    let cursor = view_of_cursor(~offset=length(prefix), ~constructable=[]);
    let tiles = view(~highlight=true, prefix @ suffix);
    Node.span([], [cursor, tiles]);
  | Some(root) =>
    let root_views =
      switch (root) {
      | OperandZ(zoperand) =>
        Code.view_of_Operand(view_of_zoperand(zoperand))
      | PreOpZ_op(zpreop, arg) =>
        Code.view_of_PreOp(view_of_zpreop(zpreop), view(arg))
      | PreOpZ_arg(preop, zarg) =>
        Code.view_of_PreOp(view_of_preop(preop), view_z(zarg))
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
    let status_view = view_of_status(ZExp.erase(ze));
    Node.span([], [status_view, ...root_views]);
  }
and view_of_zoperand =
  fun
  | ParenZ_body(zbody) => Code.view_of_Paren(view_z(zbody))
and view_of_zpreop =
  fun
  | LamZ_pat(_, zp) => Code.view_of_Lam(Code_Pat.view_z(zp))
and view_of_zpostop =
  fun
  | ApZ_arg(_, zarg) => Code.view_of_Ap(view_z(zarg));
