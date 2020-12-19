open Virtual_dom.Vdom;
open Util;
open Core;

let space = ListUtil.join(Node.text(Unicode.nbsp));

// operand
let of_OperandHole = Node.text(Unicode.nbsp);
let of_Var = x => Node.text(x);
let of_NumLit = n => Node.text(string_of_int(n));
let of_Paren = (
  Node.span([Attr.classes(["paren"])], [Node.text("(")]),
  Node.span([Attr.classes(["paren"])], [Node.text(")")]),
);

// preop
let of_Lam = (Node.text(Unicode.lam), Node.text("."));
let of_Let = (Node.text("let"), Node.text("="), Node.text("in"));

// postop
let of_Ann = (Node.text(":"), Node.text(""));
let of_Ap = of_Paren;

// binop
let of_Arrow = Node.text(Unicode.right_arrow);
let of_Plus = Node.text("+");
let of_OperatorHole = Node.text(Unicode.nbsp);

module type SORT_INPUT = {
  module T: Tile.S;
  module Z: ZTile.S with module T := T;

  let view_of_tile: T.t => list(Node.t);
  let view_of_ztile: Z.ztile => ZView.t(unit);

  let view_of_selection_tile:
    ((ZPath.child_step, ZPath.ordered_selection), T.t) =>
    ZView.t(list(Node.t));
};

module type COMMON = {
  module T: Tile.S;
  module Z: ZTile.S with module T := T;

  let view: T.s => list(Node.t);
  let view_of_z: Z.t => ZView.t(unit);
  let view_of_unzipped: Z.unzipped => ZView.t(unit);

  let view_of_selection:
    (ZPath.ordered_selection, T.s) => ZView.t(list(Node.t));
  let view_of_selection_zipper:
    (ZPath.ordered_selection, Z.zipper) => ZView.t(list(Node.t));
};

module type TYP = {
  include SORT_INPUT with module T := HTyp.T and module Z := ZTyp;
  include COMMON with module T := HTyp.T and module Z := ZTyp;
};
module type PAT = {
  include SORT_INPUT with module T := HPat.T and module Z := ZPat;
  include COMMON with module T := HPat.T and module Z := ZPat;
};
module type EXP = {
  include SORT_INPUT with module T := HExp.T and module Z := ZExp;
  include COMMON with module T := HExp.T and module Z := ZExp;
};

module Make =
       (
         T: Tile.S,
         Z: ZTile.S with module T := T,
         S: SORT_INPUT with module T := T and module Z := Z,
       ) => {
  let view = ts => ts |> List.map(S.view_of_tile) |> List.flatten;

  let view_of_unzipped = (unzipped: Z.unzipped): ZView.t(unit) =>
    switch (unzipped) {
    | None => ZList.mk(~z=(), ())
    | Some(ztile) => S.view_of_ztile(ztile)
    };

  let view_of_z = ({prefix, suffix, z}: Z.t): ZView.t(unit) => {
    let ZList.{prefix: pre, z: (), suffix: suf} = view_of_unzipped(z);
    let prefix = pre @ view(prefix);
    let suffix = view(suffix) @ suf;
    ZList.mk(~prefix, ~z=(), ~suffix, ());
  };

  let view_of_selection = (selection, ts) => {
    let ((steps_l, j_l), (steps_r, j_r)) = selection;
    switch (steps_l, steps_r) {
    | ([], []) =>
      let (prefix, z, suffix) =
        TupleUtil.map3(view, ListUtil.split_sublist(j_l, j_r, ts));
      ZList.mk(~prefix, ~z, ~suffix, ());
    | ([], [(tile_step_r, child_step_r), ...steps_r]) =>
      let (prefix, tile, suffix) = ListUtil.split_nth(tile_step_r, ts);
      let (prefix, selected) = ListUtil.split_n(j_l, prefix);
      let (prefix, selected, suffix) =
        TupleUtil.map3(view, (prefix, selected, suffix));
      let ZList.{prefix: selected_r, z: _, suffix: unselected_r} = {
        let r = (steps_r, j_r);
        S.view_of_selection_tile((child_step_r, (r, r)), tile);
      };
      ZList.mk(
        ~prefix,
        ~z=selected @ selected_r,
        ~suffix=unselected_r @ suffix,
        (),
      );
    | ([(tile_step_l, child_step_l), ...steps_l], []) =>
      let (prefix, tile, suffix) = ListUtil.split_nth(tile_step_l, ts);
      let (selected, suffix) =
        ListUtil.split_n(j_r - List.length(prefix) - 1, suffix);
      let (prefix, selected, suffix) =
        TupleUtil.map3(view, (prefix, selected, suffix));
      let ZList.{prefix: unselected_l, z: _, suffix: selected_l} = {
        let l = (steps_l, j_l);
        S.view_of_selection_tile((child_step_l, (l, l)), tile);
      };
      ZList.mk(
        ~prefix=prefix @ unselected_l,
        ~z=selected_l @ selected,
        ~suffix,
        (),
      );
    | (
        [(tile_step_l, child_step_l), ...steps_l],
        [(tile_step_r, child_step_r), ...steps_r],
      ) =>
      let (prefix, tile_r, suffix) = ListUtil.split_nth(tile_step_r, ts);
      let (prefix, tile_l, mid) = ListUtil.split_nth(tile_step_l, prefix);
      let (prefix, mid, suffix) =
        TupleUtil.map3(view, (prefix, mid, suffix));
      let ZList.{prefix: unselected_l, z: _, suffix: selected_l} = {
        let l = (steps_l, j_l);
        S.view_of_selection_tile((child_step_l, (l, l)), tile_l);
      };
      let ZList.{prefix: selected_r, z: _, suffix: unselected_r} = {
        let r = (steps_r, j_r);
        S.view_of_selection_tile((child_step_r, (r, r)), tile_r);
      };
      ZList.mk(
        ~prefix=prefix @ unselected_l,
        ~z=selected_l @ mid @ selected_r,
        ~suffix=unselected_r @ suffix,
        (),
      );
    };
  };

  let view_of_selection_zipper = (selection, (zipped, unzipped)) => {
    let ZList.{prefix: pre, z: (), suffix: suf} = view_of_unzipped(unzipped);
    let ZList.{prefix, z, suffix} = view_of_selection(selection, zipped);
    ZList.mk(~prefix=pre @ prefix, ~z, ~suffix=suffix @ suf, ());
  };
};

module rec Typ: TYP = {
  open HTyp.T;
  open ZTyp;

  let view_of_tile =
    Tile.get(
      fun
      | OperandHole => [of_OperandHole]
      | Num => [Node.text("num")]
      | Bool => [Node.text("bool")]
      | Paren(body) => {
          let (open_, close) = of_Paren;
          [open_, ...Typ.view(body)] @ [close];
        },
      () => raise(Void_PreOp),
      () => raise(Void_PostOp),
      fun
      | OperatorHole => [of_OperatorHole]
      | Arrow => [of_Arrow],
    );

  let view_of_ztile =
    Tile.get(
      fun
      | ParenZ_body(zty) => {
          let (l, r) = of_Paren;
          ZView.add([l], [r], Typ.view_of_z(zty));
        },
      () => raise(Void_ZPreOp),
      fun
      | AnnZ_ann(_, zp) => {
          let (l, r) = of_Ann;
          ZView.add([l], [r], Pat.view_of_z(zp));
        },
      () => raise(Void_ZBinOp),
    );

  let view_of_selection_tile =
      ((child_step, selection), tile: HTyp.T.t): ZView.t(list(Node.t)) => {
    let `Typ(zipper) = ZPath.Typ.unzip_tile(child_step, tile, ZTyp.mk());
    Typ.view_of_selection_zipper(selection, zipper);
  };

  include Make(HTyp.T, ZTyp, Typ);
}
and Pat: PAT = {
  open HPat.T;
  open ZPat;

  let view_of_tile =
    Tile.get(
      fun
      | OperandHole => [of_OperandHole]
      | Var(x) => [of_Var(x)]
      | Paren(body) => {
          let (open_, close) = of_Paren;
          [open_, ...Pat.view(body)] @ [close];
        },
      () => raise(Void_PreOp),
      fun
      | Ann(_, ann) => {
          let (open_, close) = of_Ann;
          [open_, ...Typ.view(ann)] @ [close];
        },
      fun
      | OperatorHole => [of_OperatorHole],
    );

  let view_of_ztile =
    Tile.get(
      fun
      | ParenZ_body(zp) => {
          let (l, r) = of_Paren;
          ZView.add([l], [r], Pat.view_of_z(zp));
        },
      fun
      | LamZ_pat(_, ze) => {
          let (l, r) = of_Lam;
          ZView.add([l], [r], Exp.view_of_z(ze));
        }
      | LetZ_pat(ze, def) => {
          let (let_, eq, in_) = of_Let;
          let def = Exp.view(def);
          ZView.add([let_], [eq, ...def] @ [in_], Exp.view_of_z(ze));
        },
      () => raise(Void_ZPostOp),
      () => raise(Void_ZBinOp),
    );

  let view_of_selection_tile = ((child_step, selection), tile) =>
    switch (ZPath.Pat.unzip_tile(child_step, tile, ZPat.mk())) {
    | `Typ(zipper) => Typ.view_of_selection_zipper(selection, zipper)
    | `Pat(zipper) => Pat.view_of_selection_zipper(selection, zipper)
    };

  include Make(HPat.T, ZPat, Pat);
}
and Exp: EXP = {
  open HExp.T;
  open ZExp;

  let view_of_tile =
    Tile.get(
      fun
      | OperandHole => [of_OperandHole]
      | Var(_, x) => [of_Var(x)]
      | Num(_, n) => [of_NumLit(n)]
      | Paren(body) => {
          let (open_, close) = of_Paren;
          [open_, ...Exp.view(body)] @ [close];
        },
      fun
      | Lam(_, p) => {
          let (lam, dot) = of_Lam;
          [lam, ...Pat.view(p)] @ [dot];
        }
      | Let(p, def) => {
          let (let_, eq, in_) = of_Let;
          [let_, ...Pat.view(p)] @ [eq, ...Exp.view(def)] @ [in_];
        },
      fun
      | Ap(_, arg) => {
          let (open_, close) = of_Ap;
          [open_, ...Exp.view(arg)] @ [close];
        },
      fun
      | OperatorHole => [of_OperatorHole]
      | Plus(_) => [of_Plus],
    );

  let view_of_ztile =
    Tile.get(
      fun
      | ParenZ_body(ze) => {
          let (l, r) = of_Paren;
          ZView.add([l], [r], Exp.view_of_z(ze));
        },
      fun
      | LetZ_def(p, ze) => {
          let (let_, eq, in_) = of_Let;
          let p = Pat.view(p);
          ZView.add([let_, ...p] @ [eq], [in_], Exp.view_of_z(ze));
        },
      fun
      | ApZ_arg(_, ze) => {
          let (l, r) = of_Paren;
          ZView.add([l], [r], Exp.view_of_z(ze));
        },
      () =>
      raise(ZExp.Void_ZBinOp)
    );

  let view_of_selection_tile = ((child_step, selection), tile) =>
    switch (ZPath.Exp.unzip_tile(child_step, tile, ZExp.mk())) {
    | `Pat(zipper) => Pat.view_of_selection_zipper(selection, zipper)
    | `Exp(zipper) => Exp.view_of_selection_zipper(selection, zipper)
    };

  include Make(HExp.T, ZExp, Exp);
};
