open Virtual_dom.Vdom;
open Util;
open Core;

let hole_radii = (~font_metrics: FontMetrics.t) => {
  let r = 3.5;
  (r /. font_metrics.col_width, r /. font_metrics.row_height);
};

let decoration_container =
    (
      ~font_metrics: FontMetrics.t,
      ~origin: int=0,
      ~length: int,
      ~cls: string,
      svgs: list(Node.t),
    )
    : Node.t => {
  let buffered_height = 2;
  let buffered_width = length + 1;

  let buffered_height_px =
    Float.of_int(buffered_height) *. font_metrics.row_height;
  let buffered_width_px =
    Float.of_int(buffered_width) *. font_metrics.col_width;

  let container_origin_x =
    (Float.of_int(origin) -. 0.5) *. font_metrics.col_width;
  let container_origin_y = (-0.5) *. font_metrics.row_height;

  Node.div(
    [
      Attr.classes([
        "decoration-container",
        Printf.sprintf("%s-container", cls),
      ]),
      Attr.create(
        "style",
        Printf.sprintf(
          "top: %fpx; left: %fpx;",
          container_origin_y,
          container_origin_x,
        ),
      ),
    ],
    [
      Node.create_svg(
        "svg",
        [
          Attr.classes([cls]),
          Attr.create(
            "viewBox",
            Printf.sprintf(
              "-0.5 -0.5 %d %d",
              buffered_width,
              buffered_height,
            ),
          ),
          Attr.create("width", Printf.sprintf("%fpx", buffered_width_px)),
          Attr.create("height", Printf.sprintf("%fpx", buffered_height_px)),
          Attr.create("preserveAspectRatio", "none"),
        ],
        svgs,
      ),
    ],
  );
};

module type COMMON = {
  type tiles;
  type ztile;
  let length: tiles => int;
  let offset: (ZPath.t, tiles) => int;
  let empty_holes: tiles => list(int);
  let view_of_ztile: ztile => (list(Node.t), list(Node.t));
  let view_of_normal:
    (~font_metrics: FontMetrics.t, ZPath.t, (tiles, option(ztile))) =>
    list(Node.t);
  let view: (~font_metrics: FontMetrics.t, EditState.Mode.t, tiles) => Node.t;
};

let space = 1;
module Common =
       (
         T: Tile.S,
         V: {
           // TODO make Z sig
           type ztile;
           let length_of_tile: T.t => int;
           let offset_tile: ((ZPath.child_step, ZPath.t), T.t) => int;
           let text_of_tile: T.t => Node.t;
           let profile_of_tile: T.t => CodeDecoration.Tile.profile;
           let empty_holes_of_tile: T.t => list(int);
           let view_of_normal:
             (
               ~font_metrics: FontMetrics.t,
               (ZPath.two_step, ZPath.t),
               (T.s, option(ztile))
             ) =>
             list(Node.t);
         },
       ) => {
  let length = (ts: T.s): int =>
    ts
    |> List.map(V.length_of_tile)
    |> List.map((+)(1))
    |> List.fold_left((+), -1);

  let offset = ((steps, j): ZPath.t, ts: T.s) =>
    switch (steps) {
    | [] =>
      let (prefix, _) = ListUtil.split_n(j, ts);
      length(prefix);
    | [(tile_step, child_step), ...steps] =>
      let (prefix, tile, _) = ListUtil.split_nth(tile_step, ts);
      length(prefix)
      + space
      + V.offset_tile((child_step, (steps, j)), tile);
    };

  let empty_holes = (ts: T.s): list(int) => {
    let (_, holes) =
      ts
      |> ListUtil.fold_left_map(
           (start, tile) => {
             let origins =
               V.empty_holes_of_tile(tile) |> List.map((+)(start));
             (start + V.length_of_tile(tile) + space, origins);
           },
           0,
         );
    List.concat(holes);
  };

  let text = (ts: T.s): Node.t => {
    let tiles = List.map(V.text_of_tile, ts);
    Node.span([], ListUtil.join(Node.text(Unicode.nbsp), tiles));
  };

  let view_of_decorated_tile =
      (~font_metrics: FontMetrics.t, tile: T.t): Node.t => {
    let hole_radii = hole_radii(~font_metrics);
    let text = V.text_of_tile(tile);
    let decoration = {
      let profile = V.profile_of_tile(tile);
      decoration_container(
        ~font_metrics,
        ~length=profile.len,
        ~cls="tile",
        CodeDecoration.Tile.view(~sort=T.sort, ~hole_radii, profile),
      );
    };
    Node.span([Attr.classes(["decorated-tile"])], [text, decoration]);
  };

  let view_of_decorated_open_child =
      (~font_metrics: FontMetrics.t, ~side: Direction.t, ts: T.s): Node.t => {
    let text = text(ts);
    let contour = {
      let length = length(ts);
      decoration_container(
        ~font_metrics,
        ~length=length + 1,
        ~origin=
          switch (side) {
          | Left => 0
          | Right => (-1)
          },
        ~cls="open-child",
        CodeDecoration.OpenChild.view(~sort=T.sort, ~side, length),
      );
    };
    /*
     let inset_empty_holes = {
       let radii = hole_radii(~font_metrics);
       empty_holes(e)
       |> List.map(origin =>
            decoration_container(
              ~font_metrics,
              ~origin,
              ~length=1,
              ~cls="inset-empty-hole",
              CodeDecoration.EmptyHole.view(~radii, ~inset=true),
            )
          );
     };
     */
    Node.span(
      [Attr.classes(["decorated-open-child"])],
      // [text, contour, ...inset_empty_holes],
      [text, contour],
    );
  };

  module Ts = Tiles.Make(T);
  let view_of_decorated_term =
      (~font_metrics: FontMetrics.t, root: Ts.root): Node.t => {
    let view_of_decorated_tile = view_of_decorated_tile(~font_metrics);
    let view_of_decorated_open_child =
      view_of_decorated_open_child(~font_metrics);
    let vs =
      switch (root) {
      | Operand(operand) => [view_of_decorated_tile(Operand(operand))]
      | PreOp((preop, r)) => [
          view_of_decorated_tile(PreOp(preop)),
          view_of_decorated_open_child(~side=Right, r),
        ]
      | PostOp((l, postop)) => [
          view_of_decorated_open_child(~side=Left, l),
          view_of_decorated_tile(PostOp(postop)),
        ]
      | BinOp((l, binop, r)) => [
          view_of_decorated_open_child(~side=Left, l),
          view_of_decorated_tile(BinOp(binop)),
          view_of_decorated_open_child(~side=Right, r),
        ]
      };
    Node.span([Attr.classes(["decorated-term"])], CodeText.space(vs));
  };

  let view_of_normal =
      (
        ~font_metrics: FontMetrics.t,
        (steps, j): ZPath.t,
        (ts: T.s, _: option(V.ztile)) as zipper,
      )
      : list(Node.t) => {
    let view_of_decorated_term = view_of_decorated_term(~font_metrics);
    switch (steps) {
    | [] =>
      let caret = {
        let (prefix, _) = ListUtil.split_n(j, ts);
        let len = length(prefix);
        CodeDecoration.Caret.view(~font_metrics, len, []);
      };
      let code = {
        let k = j == List.length(ts) ? j - 1 : j;
        let ZList.{prefix, z, suffix} = Ts.nth_root(k, ts);
        let prefix = List.map(V.text_of_tile, prefix);
        let zroot = view_of_decorated_term(z);
        let suffix = List.map(V.text_of_tile, suffix);
        CodeText.space(prefix @ [zroot, ...suffix]);
      };
      [Node.span([Attr.classes(["zipped"])], [caret, ...code])];
    | [two_step, ...steps] =>
      V.view_of_normal(~font_metrics, (two_step, (steps, j)), zipper)
    };
  };
};

module type TYP = COMMON with type tiles = HTyp.t and type ztile = ZTyp.ztile;
module rec Typ: TYP = {
  type tiles = HTyp.t;
  type ztile = ZTyp.ztile;

  module V = {
    open HTyp.Tile;
    type ztile = ZTyp.ztile;

    let length_of_tile: t => int =
      Tile.map(
        fun
        | OperandHole => 1
        | Num => 3
        | Paren(body) => 1 + space + Typ.length(body) + space + 1,
        () => raise(Void_PreOp),
        () => raise(Void_PostOp),
        fun
        | OperatorHole
        | Arrow => 1,
      );

    let offset_tile = ((child_step, path), tile) => {
      let `Typ(ty, unzipped) =
        ZPath.Typ.unzip_tile(child_step, tile, ZTyp.mk());
      switch (Option.get(unzipped)) {
      | Operand(ParenZ_body(_)) => 1 + space + Typ.offset(path, ty)
      | PreOp(_) => raise(ZTyp.Void_ZPreOp)
      | PostOp(AnnZ_ann(_)) =>
        failwith("rezipping unzipped result would change sort")
      | BinOp(_) => raise(ZTyp.Void_ZBinOp)
      };
    };

    let text_of_tile = CodeText.Typ.view_of_tile;
    let profile_of_tile = _ => failwith("todo");

    let empty_holes_of_tile = {
      let shift = n => List.map((+)(n + space));
      Tile.map(
        fun
        | OperandHole => [0]
        | Num => []
        | Paren(body) => shift(1, Typ.empty_holes(body)),
        () => raise(Void_PreOp),
        () => raise(Void_PostOp),
        fun
        | OperatorHole => [0]
        | Arrow => [],
      );
    };

    let view_of_normal = (~font_metrics, (two_step, path), zipper) => {
      let `Typ(zipper) = ZPath.Typ.unzip(two_step, zipper);
      Typ.view_of_normal(~font_metrics, path, zipper);
    };
  };
  include Common(HTyp.Tile, V);

  let view_of_ztile = _ => failwith("todo");
  let view = (~font_metrics as _, _, _) => failwith("todo");
};

module type PAT = {
  include COMMON with type tiles = HPat.t and type ztile = ZPat.ztile;
  let err_holes: HPat.t => list((int, CodeDecoration.ErrHole.profile));
  let err_holes_z:
    (ZPath.t, HPat.t) => list((int, CodeDecoration.ErrHole.profile));
};
module rec Pat: PAT = {
  type tiles = HPat.t;
  type ztile = ZPat.ztile;

  module V = {
    open HPat.Tile;
    type ztile = ZPat.ztile;

    let length_of_tile: t => int =
      Tile.map(
        fun
        | OperandHole => 1
        | Var(x) => String.length(x)
        | Paren(body) => 1 + space + Pat.length(body) + space + 1,
        () => raise(Void_PreOp),
        fun
        | Ann(_, ann) => 1 + space + Typ.length(ann),
        fun
        | OperatorHole => 1,
      );

    let offset_tile = ((child_step, path), tile) =>
      switch (ZPath.Pat.unzip_tile(child_step, tile, ZPat.mk())) {
      | `Typ(ty, unzipped) =>
        switch (Option.get(unzipped)) {
        | Operand(ParenZ_body(_)) =>
          failwith("rezipping unzipped result would change sort")
        | PreOp(_) => raise(ZTyp.Void_ZPreOp)
        | PostOp(AnnZ_ann(_)) => 1 + space + Typ.offset(path, ty)
        | BinOp(_) => raise(ZTyp.Void_ZBinOp)
        }
      | `Pat(p, unzipped) =>
        switch (Option.get(unzipped)) {
        | Operand(ParenZ_body(_)) => 1 + space + Pat.offset(path, p)
        | PreOp(LamZ_pat(_)) =>
          failwith("rezipping unzipped result would change sort")
        | PostOp(_) => raise(ZPat.Void_ZPostOp)
        | BinOp(_) => raise(ZPat.Void_ZBinOp)
        }
      };

    let text_of_tile = CodeText.Pat.view_of_tile;
    let profile_of_tile = _ => failwith("todo");

    let empty_holes_of_tile = {
      let shift = n => List.map((+)(n + space));
      Tile.map(
        fun
        | OperandHole => [0]
        | Var(_) => []
        | Paren(body) => shift(1, Pat.empty_holes(body)),
        () => raise(Void_PreOp),
        fun
        | Ann(_, ty) => shift(1, Typ.empty_holes(ty)),
        fun
        | OperatorHole => [0],
      );
    };

    let view_of_normal = (~font_metrics, (two_step, path), zipper) =>
      switch (ZPath.Pat.unzip(two_step, zipper)) {
      | `Typ(zipper) => Typ.view_of_normal(~font_metrics, path, zipper)
      | `Pat(zipper) => Pat.view_of_normal(~font_metrics, path, zipper)
      };
  };
  include Common(HPat.Tile, V);

  let err_holes = _ => failwith("todo");
  let err_holes_z = (_, _) => failwith("todo");

  let view_of_ztile = _ => failwith("todo");
  let view_of_normal = (~font_metrics as _, _, _) => failwith("todo");
  let view = (~font_metrics as _, _, _) => failwith("todo");
};

module type EXP = {
  include COMMON with type tiles = HExp.t and type ztile = ZExp.ztile;
  let err_holes:
    (~expanded: bool=?, HExp.t) =>
    list((int, CodeDecoration.ErrHole.profile));
  let err_holes_z:
    (ZPath.t, HExp.t) => list((int, CodeDecoration.ErrHole.profile));
};
module rec Exp: EXP = {
  type tiles = HExp.t;
  type ztile = ZExp.ztile;

  module V = {
    open HExp.Tile;
    type ztile = ZExp.ztile;

    let length_of_tile =
      Tile.map(
        fun
        | OperandHole => 1
        | Var(_, x) => String.length(x)
        | Num(_, n) => String.length(string_of_int(n))
        | Paren(body) => 1 + space + Exp.length(body) + space + 1,
        fun
        | Lam(_, p) => 1 + space + Pat.length(p) + space + 1,
        fun
        | Ap(_, arg) => 1 + space + Exp.length(arg) + space + 1,
        fun
        | OperatorHole
        | Plus(_) => 1,
      );

    let offset_tile = ((child_step, path), tile) =>
      switch (ZPath.Exp.unzip_tile(child_step, tile, ZExp.mk())) {
      | `Pat(p, unzipped) =>
        switch (Option.get(unzipped)) {
        | Operand(ParenZ_body(_)) =>
          failwith("rezipping unzipped result would change sort")
        | PreOp(LamZ_pat(_)) => 1 + space + Pat.offset(path, p)
        | PostOp(_) => raise(ZPat.Void_ZPostOp)
        | BinOp(_) => raise(ZPat.Void_ZBinOp)
        }
      | `Exp(e, unzipped) =>
        switch (Option.get(unzipped)) {
        | Operand(ParenZ_body(_))
        | PostOp(ApZ_arg(_)) => 1 + space + Exp.offset(path, e)
        | PreOp(_) => raise(ZExp.Void_ZPreOp)
        | BinOp(_) => raise(ZExp.Void_ZBinOp)
        }
      };

    let text_of_tile = CodeText.Exp.view_of_tile;

    let profile_of_tile = (tile: HExp.Tile.t): CodeDecoration.Tile.profile => {
      switch (tile) {
      | Operand(operand) =>
        let (open_children, closed_children, len, is_hole) =
          switch (operand) {
          | OperandHole => ([], [], 1, true)
          | Var(_)
          | Num(_) => ([], [], length_of_tile(Operand(operand)), false)
          | Paren(body) =>
            let body_len = Exp.length(body);
            ([(2, body_len)], [], 2 + body_len + 2, false);
          };
        {shape: `Operand(is_hole), len, open_children, closed_children};
      | PreOp(preop) =>
        let (open_children, closed_children, len) =
          switch (preop) {
          | Lam(_, p) =>
            let p_len = Pat.length(p);
            ([], [(2, p_len)], 2 + p_len + 2);
          };
        {shape: `PreOp, len, open_children, closed_children};
      | PostOp(postop) =>
        let (open_children, closed_children, len) =
          switch (postop) {
          | Ap(_, arg) =>
            let arg_len = Exp.length(arg);
            ([], [(2, arg_len)], 2 + arg_len + 2);
          };
        {shape: `PostOp, len, open_children, closed_children};
      | BinOp(binop) =>
        let (open_children, closed_children, len, is_hole) =
          switch (binop) {
          | Plus(_) => ([], [], 1, false)
          | OperatorHole => ([], [], 1, true)
          };
        CodeDecoration.Tile.{
          shape: `BinOp(is_hole),
          len,
          open_children,
          closed_children,
        };
      };
    };

    let empty_holes_of_tile = {
      let shift = n => List.map((+)(n + space));
      Tile.map(
        fun
        | OperandHole => [0]
        | Num(_)
        | Var(_) => []
        | Paren(body) => shift(1, Exp.empty_holes(body)),
        fun
        | Lam(_, p) => shift(1, Pat.empty_holes(p)),
        fun
        | Ap(_, arg) => shift(1, Exp.empty_holes(arg)),
        fun
        | OperatorHole => [0]
        | Plus(_) => [],
      );
    };

    let view_of_normal = (~font_metrics, (two_step, path), zipper) =>
      switch (ZPath.Exp.unzip(two_step, zipper)) {
      | `Pat(zipper) => Pat.view_of_normal(~font_metrics, path, zipper)
      | `Exp(zipper) => Exp.view_of_normal(~font_metrics, path, zipper)
      };
  };
  include Common(HExp.Tile, V);

  let rec err_holes =
          (~expanded=false, e: HExp.t)
          : list((int, CodeDecoration.ErrHole.profile)) => {
    let shift = n => List.map(PairUtil.map_fst((+)(n)));
    let outer_hole =
      switch (HExp.get_hole_status(e)) {
      | NotInHole => []
      | InHole =>
        let len = length(e);
        [
          {
            (0, CodeDecoration.ErrHole.{expanded, len});
          },
        ];
      };
    let inner_holes =
      switch (HExp.root(e)) {
      | Operand(OperandHole | Num(_) | Var(_)) => []
      | Operand(Paren(body)) => err_holes(body)
      | PreOp((Lam(_, p) as preop, body)) =>
        let pat_holes = shift(1 + space, Pat.err_holes(p));
        let body_holes =
          shift(
            V.length_of_tile(PreOp(preop)) + space,
            err_holes(~expanded, body),
          );
        pat_holes @ body_holes;
      | PostOp((fn, Ap(_, arg))) =>
        let fn_holes = err_holes(~expanded, fn);
        let arg_holes = shift(1 + space, err_holes(arg));
        fn_holes @ arg_holes;
      | BinOp((l, (OperatorHole | Plus(_)) as binop, r)) =>
        let l_holes = err_holes(~expanded, l);
        let r_holes =
          shift(
            length(l) + space + V.length_of_tile(BinOp(binop)) + space,
            err_holes(~expanded, r),
          );
        l_holes @ r_holes;
      };
    outer_hole @ inner_holes;
  };

  let rec err_holes_z =
          ((steps, j): ZPath.t, e: HExp.t)
          : list((int, CodeDecoration.ErrHole.profile)) =>
    switch (steps) {
    | [] => err_holes(~expanded=true, e)
    // assuming steps are in sync
    | [(tile_step, _child_step) as two_step, ...steps] =>
      let outer_hole =
        switch (HExp.get_hole_status(e)) {
        | NotInHole => []
        | InHole => [
            (0, CodeDecoration.ErrHole.{expanded: true, len: length(e)}),
          ]
        };
      let inner_holes = {
        let shift = n => List.map(PairUtil.map_fst((+)(n)));
        switch (HExp.root(e)) {
        | Operand(OperandHole | Num(_) | Var(_)) => raise(ZPath.Out_of_sync)
        | Operand(Paren(body)) =>
          err_holes_z((steps, j), body) |> shift(2)
        | PreOp((Lam(_, p) as preop, body)) =>
          let in_preop = tile_step == 0;
          let holes_p =
            in_preop ? Pat.err_holes_z((steps, j), p) : Pat.err_holes(p);
          let holes_body =
            in_preop
              ? err_holes(body)
              : {
                let two_step = PairUtil.map_fst((-)(1), two_step);
                err_holes_z(([two_step, ...steps], j), body);
              };
          let holes_p = shift(2, holes_p);
          let holes_body =
            shift(V.length_of_tile(PreOp(preop)) + space, holes_body);
          holes_p @ holes_body;
        | PostOp((fn, Ap(_, arg))) =>
          let in_postop = tile_step == List.length(fn);
          let holes_fn =
            in_postop
              ? err_holes(fn) : err_holes_z(([two_step, ...steps], j), fn);
          let holes_arg =
            in_postop ? err_holes_z((steps, j), arg) : err_holes(arg);
          let holes_arg = shift(length(fn) + space, holes_arg);
          holes_fn @ holes_arg;
        | BinOp((l, binop, r)) =>
          let len_l = List.length(l);
          let in_l = tile_step < len_l;
          let holes_l =
            in_l
              ? err_holes_z(([two_step, ...steps], j), l) : err_holes(l);
          let holes_r =
            in_l
              ? err_holes(r)
              : {
                let two_step =
                  PairUtil.map_fst(
                    tile_step => tile_step - (len_l + 1),
                    two_step,
                  );
                err_holes_z(([two_step, ...steps], j), r);
              };
          let holes_r =
            shift(
              length(l) + space + V.length_of_tile(BinOp(binop)) + space,
              holes_r,
            );
          holes_l @ holes_r;
        };
      };
      outer_hole @ inner_holes;
    };

  let view_of_ztile = (ztile: ZExp.ztile): (list(Node.t), list(Node.t)) => {
    switch (ztile) {
    | Operand(ParenZ_body({prefix, suffix, _}))
    | PostOp(ApZ_arg(_, {prefix, suffix, _})) =>
      let prefix = List.map(CodeText.Exp.view_of_tile, prefix);
      let (l, r) = CodeText.of_Paren;
      let suffix = List.map(CodeText.Exp.view_of_tile, suffix);
      (prefix @ [l], [r, ...suffix]);
    | PreOp(_) => raise(ZExp.Void_ZPreOp)
    | BinOp(_) => raise(ZExp.Void_ZBinOp)
    };
  };

  let view_of_decorated_selection =
      (
        ~font_metrics: FontMetrics.t,
        selection: ZPath.ordered_selection,
        e: HExp.t,
      )
      : (list(Node.t), list(Node.t), list(Node.t)) => {
    let view_of_decorated_tile = view_of_decorated_tile(~font_metrics);
    let rec go = (((steps_l, j_l), (steps_r, j_r)), e: HExp.t) =>
      switch (steps_l, steps_r) {
      | ([], []) =>
        let (prefix, selected, suffix) = ListUtil.split_sublist(j_l, j_r, e);
        let prefix = List.map(CodeText.Exp.view_of_tile, prefix);
        let selected = List.map(view_of_decorated_tile, selected);
        let suffix = List.map(CodeText.Exp.view_of_tile, suffix);
        (prefix, selected, suffix);
      | ([], [two_step_r, ...steps_r]) =>
        switch (ZPath.Exp.unzip(two_step_r, (e, None))) {
        | `Pat(_) => failwith("todo")
        | `Exp(e, unzipped) =>
          switch (Option.get(unzipped)) {
          | Operand(ParenZ_body({prefix, suffix, _}))
          | PostOp(ApZ_arg(_, {prefix, suffix, _})) =>
            let (prefix, selected) = ListUtil.split_n(j_l, prefix);
            let prefix = List.map(CodeText.Exp.view_of_tile, prefix);
            let suffix = List.map(CodeText.Exp.view_of_tile, suffix);
            let selected = List.map(view_of_decorated_tile, selected);
            let (open_paren, close_paren) = CodeText.of_Paren;
            let (_, inner_selected, inner_suffix) =
              go((([], 0), (steps_r, j_r)), e);
            (
              prefix,
              selected @ [open_paren, ...inner_selected],
              inner_suffix @ [close_paren, ...suffix],
            );
          | PreOp(_) => raise(ZExp.Void_ZPreOp)
          | BinOp(_) => raise(ZExp.Void_ZBinOp)
          }
        }
      | ([two_step_l, ...steps_l], []) =>
        switch (ZPath.Exp.unzip(two_step_l, (e, None))) {
        | `Pat(_) => failwith("todo")
        | `Exp(e, unzipped) =>
          switch (Option.get(unzipped)) {
          | Operand(ParenZ_body({prefix, suffix, _}))
          | PostOp(ApZ_arg(_, {prefix, suffix, _})) =>
            let (selected, suffix) =
              ListUtil.split_n(j_r - List.length(prefix) - 1, suffix);
            let prefix = List.map(CodeText.Exp.view_of_tile, prefix);
            let suffix = List.map(CodeText.Exp.view_of_tile, suffix);
            let selected = List.map(view_of_decorated_tile, selected);
            let (open_paren, close_paren) = CodeText.of_Paren;
            let (inner_prefix, inner_selected, _) =
              go(((steps_l, j_l), ([], List.length(e))), e);
            (
              prefix @ [open_paren, ...inner_prefix],
              inner_selected @ [close_paren, ...selected],
              suffix,
            );
          | PreOp(_) => raise(ZExp.Void_ZPreOp)
          | BinOp(_) => raise(ZExp.Void_ZBinOp)
          }
        }
      | ([two_step, ...steps_l], [two_step', ...steps_r])
          when two_step == two_step' =>
        switch (ZPath.Exp.unzip(two_step, (e, None))) {
        | `Pat(_) => failwith("Code.Exp.view_of_selecting")
        | `Exp(e, unzipped) =>
          let (l, r) = view_of_ztile(Option.get(unzipped));
          let (prefix, selected, suffix) =
            go(((steps_l, j_l), (steps_r, j_r)), e);
          (l @ prefix, selected, suffix @ r);
        }
      | ([two_step_l, ...steps_l], [two_step_r, ...steps_r]) =>
        switch (
          ZPath.Exp.unzip(two_step_l, (e, None)),
          ZPath.Exp.unzip(two_step_r, (e, None)),
        ) {
        | (`Pat(_), _)
        | (_, `Pat(_)) => failwith("Code.Exp.view_of_selecting")
        | (`Exp(e_l, unzipped_l), `Exp(e_r, unzipped_r)) =>
          let (prefix, selected_l_mid) =
            switch (Option.get(unzipped_l)) {
            | Operand(ParenZ_body({prefix, suffix, _}))
            | PostOp(ApZ_arg(_, {prefix, suffix, _})) =>
              let prefix = List.map(CodeText.Exp.view_of_tile, prefix);
              let selected_mid = {
                let (mid, _) =
                  ListUtil.split_n(
                    fst(two_step_r) - List.length(prefix) - 1,
                    suffix,
                  );
                List.map(view_of_decorated_tile, mid);
              };
              let (open_paren, close_paren) = CodeText.of_Paren;
              let (inner_prefix, selected_l, _) =
                go(((steps_l, j_l), ([], List.length(e_l))), e_l);
              (
                prefix @ [open_paren, ...inner_prefix],
                selected_l @ [close_paren, ...selected_mid],
              );
            | PreOp(_) => raise(ZExp.Void_ZPreOp)
            | BinOp(_) => raise(ZExp.Void_ZBinOp)
            };
          let (selected_r, suffix) =
            switch (Option.get(unzipped_r)) {
            | Operand(ParenZ_body({suffix, _}))
            | PostOp(ApZ_arg(_, {suffix, _})) =>
              let (open_paren, close_paren) = CodeText.of_Paren;
              let (_, selected_r, inner_suffix) =
                go((([], 0), (steps_r, j_r)), e_r);
              let suffix = List.map(CodeText.Exp.view_of_tile, suffix);
              (
                [open_paren, ...selected_r],
                inner_suffix @ [close_paren, ...suffix],
              );
            | PreOp(_) => raise(ZExp.Void_ZPreOp)
            | BinOp(_) => raise(ZExp.Void_ZBinOp)
            };
          (prefix, selected_l_mid @ selected_r, suffix);
        }
      };
    go(selection, e);
  };

  let view_of_selecting =
      (
        ~font_metrics: FontMetrics.t,
        selection: ZPath.anchored_selection,
        e: HExp.t,
      )
      : Node.t => {
    let ((l, r), caret_side) = ZPath.mk_ordered_selection(selection);
    let (prefix, selection, suffix) =
      view_of_decorated_selection(~font_metrics, (l, r), e);
    let (caret, selection_box) = {
      let (offset_l, offset_r) = (offset(l, e), offset(r, e));
      let caret =
        CodeDecoration.Caret.view(
          ~font_metrics,
          caret_side == Left ? offset_l : offset_r,
          [],
        );
      let selection_box =
        Node.div(
          [
            Attr.classes(["selection-box"]),
            Attr.create(
              "style",
              Printf.sprintf(
                "left: %fpx; top: %fpx; width: %fpx; height: %fpx;",
                (Float.of_int(offset_l) +. 0.5) *. font_metrics.col_width,
                (-0.15) *. font_metrics.row_height,
                font_metrics.col_width *. Float.of_int(offset_r - offset_l),
                font_metrics.row_height *. 1.2,
              ),
            ),
          ],
          [],
        );
      (caret, selection_box);
    };
    Node.span(
      [Attr.classes(["selection-container"])],
      [caret, selection_box, ...CodeText.space(prefix @ selection @ suffix)],
    );
  };

  let view_of_restructuring =
      (
        ~font_metrics: FontMetrics.t,
        (l, r) as selection: ZPath.ordered_selection,
        target: ZPath.t,
        e: HExp.t,
      )
      : Node.t => {
    let (prefix, selected, suffix) =
      view_of_decorated_selection(~font_metrics, selection, e);
    let selection_len = offset(r, e) - offset(l, e);
    let placeholder =
      Node.span(
        [Attr.classes(["placeholder"])],
        [
          Node.div(
            [
              Attr.classes(["placeholder-mask"]),
              Attr.create(
                "style",
                Printf.sprintf(
                  "left: %fpx; top: %fpx; width: %fpx; height: %fpx;",
                  (-0.5) *. font_metrics.col_width,
                  (-0.15) *. font_metrics.row_height,
                  font_metrics.col_width *. Float.of_int(selection_len),
                  font_metrics.row_height *. 1.2,
                ),
              ),
            ],
            [],
          ),
          ...CodeText.space(selected),
        ],
      );
    let (caret, flag) = {
      let offset = offset(target, e);
      let caret = CodeDecoration.Caret.view(~font_metrics, offset, []);
      let flag =
        Node.span(
          [
            Attr.classes(["flag"]),
            Attr.create(
              "style",
              Printf.sprintf(
                "left: %fpx; top: %fpx; padding: 0 %fpx;",
                (Float.of_int(offset) +. 0.5) *. font_metrics.col_width,
                (-1.15) *. font_metrics.row_height,
                0.5 *. font_metrics.col_width,
              ),
            ),
          ],
          [
            Node.div(
              [
                Attr.classes(["selection-box", "moving"]),
                Attr.create(
                  "style",
                  Printf.sprintf(
                    "left: 0; top: %fpx; width: %fpx; height: %fpx;",
                    (-0.15) *. font_metrics.row_height,
                    font_metrics.col_width *. Float.of_int(selection_len),
                    font_metrics.row_height *. 1.2,
                  ),
                ),
              ],
              [],
            ),
            ...CodeText.space(selected),
          ],
        );
      (caret, flag);
    };
    Node.span(
      [Attr.classes(["restructuring"])],
      [caret, flag, ...CodeText.space(prefix @ [placeholder, ...suffix])],
    );
  };

  let view =
      (~font_metrics: FontMetrics.t, mode: EditState.Mode.t, e: HExp.t)
      : Node.t =>
    switch (mode) {
    | Normal(focus) =>
      Node.span(
        [],
        CodeText.space(view_of_normal(~font_metrics, focus, (e, None))),
      )
    | Selecting(selection) => view_of_selecting(~font_metrics, selection, e)
    | Restructuring(selection, target) =>
      view_of_restructuring(~font_metrics, selection, target, e)
    };
};

let empty_holes = (~font_metrics: FontMetrics.t, e: HExp.t): list(Node.t) => {
  let radii = hole_radii(~font_metrics);
  Exp.empty_holes(e)
  |> List.map(origin =>
       decoration_container(
         ~font_metrics,
         ~origin,
         ~length=1,
         ~cls="empty-hole",
         CodeDecoration.EmptyHole.view(~radii, ~inset=false),
       )
     );
};

let err_holes =
    (~font_metrics, mode: EditState.Mode.t, e: HExp.t): list(Node.t) => {
  let profiles =
    switch (mode) {
    | Normal(focus) => Exp.err_holes_z(focus, e)
    | Selecting(_) => Exp.err_holes(e)
    | Restructuring(_) => []
    };
  profiles
  |> List.map(((origin, profile: CodeDecoration.ErrHole.profile)) =>
       decoration_container(
         ~font_metrics,
         ~origin,
         ~length=profile.len,
         ~cls="err-hole",
         CodeDecoration.ErrHole.view(profile),
       )
     );
};

let view =
    (~font_metrics: FontMetrics.t, (mode, zipper) as edit_state: EditState.t) => {
  let (empty_holes, err_holes) = {
    let (zipped_mode, zipped_e) =
      switch (EditState.zip_up(edit_state)) {
      | (_, `Typ(_) | `Pat(_)) =>
        failwith("expected expression at top level")
      | (mode, `Exp(e, _)) => (mode, e)
      };
    let empty_holes = empty_holes(~font_metrics, zipped_e);
    let err_holes = err_holes(~font_metrics, zipped_mode, zipped_e);
    (empty_holes, err_holes);
  };
  let (unzipped_l, unzipped_r) = {
    let unzipped =
      switch (zipper) {
      | `Typ(_, unzipped) => Option.map(Typ.view_of_ztile, unzipped)
      | `Pat(_, unzipped) => Option.map(Pat.view_of_ztile, unzipped)
      | `Exp(_, unzipped) => Option.map(Exp.view_of_ztile, unzipped)
      };
    switch (unzipped) {
    | None => ([], [])
    | Some((l, r)) => (l, r)
    };
  };
  let zipped =
    switch (zipper) {
    | `Typ(zipped, _) => Typ.view(~font_metrics, mode, zipped)
    | `Pat(zipped, _) => Pat.view(~font_metrics, mode, zipped)
    | `Exp(zipped, _) => Exp.view(~font_metrics, mode, zipped)
    };
  Node.span(
    [Attr.id("code")],
    List.concat([
      empty_holes,
      err_holes,
      CodeText.space(unzipped_l @ [zipped, ...unzipped_r]),
    ]),
  );
};
