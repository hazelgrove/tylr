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

type zipper_view('z) = ZList.t('z, Node.t);

module type COMMON = {
  module T: Tile.S;
  module Z: ZTile.S with module T := T;
  let length: T.s => int;
  let offset: (ZPath.t, T.s) => int;
  let text_of_tile: T.t => Node.t;
  let empty_holes: T.s => list(int);
  let view_of_normal:
    (~font_metrics: FontMetrics.t, ZPath.t, Z.zipper) => zipper_view(Node.t);
  let view_of_decorated_selection:
    (~font_metrics: FontMetrics.t, ZPath.ordered_selection, Z.zipper) =>
    zipper_view((list(Node.t), list(Node.t), list(Node.t)));
  let view_of_selecting:
    (~font_metrics: FontMetrics.t, ZPath.anchored_selection, Z.zipper) =>
    zipper_view(Node.t);
  let view_of_restructuring:
    (
      ~font_metrics: FontMetrics.t,
      ZPath.ordered_selection,
      ZPath.t,
      Z.zipper
    ) =>
    zipper_view(Node.t);
  let view:
    (~font_metrics: FontMetrics.t, EditState.Mode.t, Z.zipper) => Node.t;
};

let space = 1;
module Common =
       (
         T: Tile.S,
         Z: ZTile.S with module T := T,
         V: {
           let length_of_tile: T.t => int;
           let offset_tile: ((ZPath.child_step, ZPath.t), T.t) => int;
           let text_of_tile: T.t => Node.t;
           let is_operand_hole: T.operand => bool;
           let is_operator_hole: T.binop => bool;
           let open_children_of_tile: T.t => list((int, int));
           let closed_children_of_tile: T.t => list((int, int));
           let empty_holes_of_tile: T.t => list(int);
           let view_of_ztile: Z.ztile => zipper_view(unit);
           let view_of_normal:
             (
               ~font_metrics: FontMetrics.t,
               (ZPath.two_step, ZPath.t),
               Z.zipper
             ) =>
             zipper_view(Node.t);
           let view_of_decorated_selection_tile:
             (
               ~font_metrics: FontMetrics.t,
               ~select: Direction.t,
               (ZPath.child_step, ZPath.t),
               T.t
             ) =>
             (list(Node.t), list(Node.t));
           let view_of_decorated_selection:
             (
               ~font_metrics: FontMetrics.t,
               (ZPath.two_step, ZPath.ordered_selection),
               Z.zipper
             ) =>
             zipper_view((list(Node.t), list(Node.t), list(Node.t)));
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

  let text_of_tile = V.text_of_tile;
  let text = (ts: T.s): Node.t => {
    let tiles = List.map(V.text_of_tile, ts);
    Node.span([], ListUtil.join(Node.text(Unicode.nbsp), tiles));
  };

  let profile_of_tile = (t: T.t) =>
    Decoration.Tile.{
      shape:
        switch (t) {
        | Operand(operand) => `Operand(V.is_operand_hole(operand))
        | PreOp(_) => `PreOp
        | PostOp(_) => `PostOp
        | BinOp(binop) => `BinOp(V.is_operator_hole(binop))
        },
      len: V.length_of_tile(t),
      open_children: V.open_children_of_tile(t),
      closed_children: V.closed_children_of_tile(t),
    };

  let view_of_decorated_tile =
      (~font_metrics: FontMetrics.t, tile: T.t): Node.t => {
    let hole_radii = hole_radii(~font_metrics);
    let text = V.text_of_tile(tile);
    let decoration = {
      let profile = profile_of_tile(tile);
      decoration_container(
        ~font_metrics,
        ~length=profile.len,
        ~cls="tile",
        Decoration.Tile.view(~sort=T.sort, ~hole_radii, profile),
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
        Decoration.OpenChild.view(~sort=T.sort, ~side, length),
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
              Decoration.EmptyHole.view(~radii, ~inset=true),
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
    Node.span([Attr.classes(["decorated-term"])], Text.space(vs));
  };

  let view_of_unzipped = (unzipped: Z.unzipped): zipper_view(unit) =>
    switch (unzipped) {
    | None => ZList.mk(~z=(), ())
    | Some(ztile) => V.view_of_ztile(ztile)
    };

  let view_of_normal =
      (
        ~font_metrics: FontMetrics.t,
        (steps, j): ZPath.t,
        (ts: T.s, unzipped) as zipper,
      )
      : zipper_view(Node.t) => {
    let view_of_decorated_term = view_of_decorated_term(~font_metrics);
    switch (steps) {
    | [] =>
      let ZList.{prefix, z: (), suffix} = view_of_unzipped(unzipped);
      let z = {
        let caret = {
          let (prefix, _) = ListUtil.split_n(j, ts);
          let len = length(prefix);
          Decoration.Caret.view(~font_metrics, len, []);
        };
        let code = {
          let k = j == List.length(ts) ? j - 1 : j;
          let ZList.{prefix, z, suffix} = Ts.nth_root(k, ts);
          let prefix = List.map(V.text_of_tile, prefix);
          let zroot = view_of_decorated_term(z);
          let suffix = List.map(V.text_of_tile, suffix);
          Text.space(prefix @ [zroot, ...suffix]);
        };
        Node.span([Attr.classes(["zipped"])], [caret, ...code]);
      };
      ZList.mk(~prefix, ~z, ~suffix, ());
    | [two_step, ...steps] =>
      V.view_of_normal(~font_metrics, (two_step, (steps, j)), zipper)
    };
  };

  let view_of_decorated_selection =
      (
        ~font_metrics: FontMetrics.t,
        selection: ZPath.ordered_selection,
        (ts: T.s, unzipped) as zipper,
      )
      : zipper_view((list(Node.t), list(Node.t), list(Node.t))) => {
    let ((steps_l, j_l), (steps_r, j_r)) = selection;
    let view_of_decorated_tile = view_of_decorated_tile(~font_metrics);
    switch (steps_l, steps_r) {
    | ([two_step, ...steps_l], [two_step', ...steps_r])
        when two_step == two_step' =>
      V.view_of_decorated_selection(
        ~font_metrics,
        (two_step, ((steps_l, j_l), (steps_r, j_r))),
        zipper,
      )
    | _ =>
      let ZList.{prefix, z: (), suffix} = view_of_unzipped(unzipped);
      let z =
        switch (steps_l, steps_r) {
        | ([], []) =>
          let (prefix, selected, suffix) =
            ListUtil.split_sublist(j_l, j_r, ts);
          let prefix = List.map(V.text_of_tile, prefix);
          let selected = List.map(view_of_decorated_tile, selected);
          let suffix = List.map(V.text_of_tile, suffix);
          (prefix, selected, suffix);
        | ([], [(tile_step_r, child_step_r), ...steps_r]) =>
          let (prefix, tile, suffix) = ListUtil.split_nth(tile_step_r, ts);
          let (prefix, selected) = ListUtil.split_n(j_l, prefix);
          let prefix = List.map(V.text_of_tile, prefix);
          let suffix = List.map(V.text_of_tile, suffix);
          let selected = List.map(view_of_decorated_tile, selected);
          let (inner_selected, inner_unselected) =
            V.view_of_decorated_selection_tile(
              ~font_metrics,
              ~select=Left,
              (child_step_r, (steps_r, j_r)),
              tile,
            );
          (prefix, selected @ inner_selected, inner_unselected @ suffix);
        | ([(tile_step_l, child_step_l), ...steps_l], []) =>
          let (prefix, tile, suffix) = ListUtil.split_nth(tile_step_l, ts);
          let (selected, suffix) =
            ListUtil.split_n(j_r - List.length(prefix) - 1, suffix);
          let prefix = List.map(V.text_of_tile, prefix);
          let suffix = List.map(V.text_of_tile, suffix);
          let selected = List.map(view_of_decorated_tile, selected);
          let (inner_unselected, inner_selected) =
            V.view_of_decorated_selection_tile(
              ~font_metrics,
              ~select=Right,
              (child_step_l, (steps_l, j_l)),
              tile,
            );
          (prefix @ inner_unselected, inner_selected @ selected, suffix);
        | (
            [(tile_step_l, child_step_l), ...steps_l],
            [(tile_step_r, child_step_r), ...steps_r],
          ) =>
          let (prefix, tile_r, suffix) = ListUtil.split_nth(tile_step_r, ts);
          let (prefix, tile_l, mid) =
            ListUtil.split_nth(tile_step_l, prefix);
          let prefix = List.map(V.text_of_tile, prefix);
          let (unselected_l, selected_l) =
            V.view_of_decorated_selection_tile(
              ~font_metrics,
              ~select=Right,
              (child_step_l, (steps_l, j_l)),
              tile_l,
            );
          let mid = List.map(view_of_decorated_tile, mid);
          let (selected_r, unselected_r) =
            V.view_of_decorated_selection_tile(
              ~font_metrics,
              ~select=Left,
              (child_step_r, (steps_r, j_r)),
              tile_r,
            );
          let suffix = List.map(V.text_of_tile, suffix);
          (
            prefix @ unselected_l,
            selected_l @ mid @ selected_r,
            unselected_r @ suffix,
          );
        };
      ZList.mk(~prefix, ~z, ~suffix, ());
    };
  };

  let view_of_selecting =
      (
        ~font_metrics: FontMetrics.t,
        selection: ZPath.anchored_selection,
        (ts, _) as zipper,
      ) => {
    let ((l, r), caret_side) = ZPath.mk_ordered_selection(selection);
    let ZList.{prefix, z: (pre, selected, suf), suffix} =
      view_of_decorated_selection(~font_metrics, (l, r), zipper);
    let (caret, selection_box) = {
      let (offset_l, offset_r) = (offset(l, ts), offset(r, ts));
      let caret =
        Decoration.Caret.view(
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
    let z =
      Node.span(
        [Attr.classes(["selection-container"])],
        [caret, selection_box, ...Text.space(pre @ selected @ suf)],
      );
    ZList.mk(~prefix, ~z, ~suffix, ());
  };

  let view_of_restructuring =
      (
        ~font_metrics: FontMetrics.t,
        (l, r) as selection: ZPath.ordered_selection,
        target: ZPath.t,
        (ts, _) as zipper,
      )
      : zipper_view(Node.t) => {
    let ZList.{prefix, z: (pre, selected, suf), suffix} =
      view_of_decorated_selection(~font_metrics, selection, zipper);
    let selection_len = offset(r, ts) - offset(l, ts);
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
          ...Text.space(selected),
        ],
      );
    let (caret, flag) = {
      let offset = offset(target, ts);
      let caret = Decoration.Caret.view(~font_metrics, offset, []);
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
            ...Text.space(selected),
          ],
        );
      (caret, flag);
    };
    let z =
      Node.span(
        [Attr.classes(["restructuring"])],
        [caret, flag, ...Text.space(pre @ [placeholder, ...suf])],
      );
    ZList.mk(~prefix, ~z, ~suffix, ());
  };

  let view =
      (~font_metrics: FontMetrics.t, mode: EditState.Mode.t, zipper): Node.t => {
    let ZList.{prefix, z, suffix} =
      switch (mode) {
      | Normal(focus) => view_of_normal(~font_metrics, focus, zipper)
      | Selecting(selection) =>
        view_of_selecting(~font_metrics, selection, zipper)
      | Restructuring(selection, target) =>
        view_of_restructuring(~font_metrics, selection, target, zipper)
      };
    Node.span([], Text.space(prefix @ [z, ...suffix]));
  };
};

module ErrHole =
       (
         T: Tile.S,
         V: {
           let length: T.s => int;
           let get_hole_status: T.s => HoleStatus.t;
           let inner_err_holes:
             T.s => list((int, Decoration.ErrHole.profile));
           let inner_err_holes_z:
             (ZPath.t, T.s) => list((int, Decoration.ErrHole.profile));
         },
       ) => {
  let err_holes = (ts: T.s) => {
    let outer_hole: list((int, Decoration.ErrHole.profile)) =
      switch (V.get_hole_status(ts)) {
      | NotInHole => []
      | InHole => [(0, {expanded: false, len: V.length(ts)})]
      };
    let inner_holes = V.inner_err_holes(ts);
    outer_hole @ inner_holes;
  };

  let err_holes_z = (path: ZPath.t, ts: T.s) => {
    let outer_hole: list((int, Decoration.ErrHole.profile)) =
      switch (V.get_hole_status(ts)) {
      | NotInHole => []
      | InHole => [(0, {expanded: true, len: V.length(ts)})]
      };
    let inner_holes = V.inner_err_holes_z(path, ts);
    outer_hole @ inner_holes;
  };
};

module type TYP = COMMON with module T := HTyp.Tile and module Z := ZTyp;
module type PAT = {
  include COMMON with module T := HPat.Tile and module Z := ZPat;
  let err_holes: HPat.t => list((int, Decoration.ErrHole.profile));
  let err_holes_z:
    (ZPath.t, HPat.t) => list((int, Decoration.ErrHole.profile));
};
module type EXP = {
  include COMMON with module T := HExp.Tile and module Z := ZExp;
  let err_holes: HExp.t => list((int, Decoration.ErrHole.profile));
  let err_holes_z:
    (ZPath.t, HExp.t) => list((int, Decoration.ErrHole.profile));
};

module rec Typ: TYP = {
  module V = {
    open HTyp.Tile;

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
      | PreOp () => raise(ZTyp.Void_ZPreOp)
      | PostOp(AnnZ_ann(_)) => raise(ZPath.Unzip_rezip_changes_sort)
      | BinOp () => raise(ZTyp.Void_ZBinOp)
      };
    };

    let text_of_tile = Text.Typ.view_of_tile;

    let is_operand_hole =
      fun
      | OperandHole => true
      | _ => false;
    let is_operator_hole =
      fun
      | OperatorHole => true
      | _ => false;
    let open_children_of_tile =
      Tile.map(
        fun
        | OperandHole
        | Num => []
        | Paren(body) => [(1 + space, Typ.length(body))],
        () => raise(HTyp.Tile.Void_PreOp),
        () => raise(HTyp.Tile.Void_PostOp),
        fun
        | Arrow
        | OperatorHole => [],
      );
    let closed_children_of_tile =
      Tile.map(
        fun
        | OperandHole
        | Num
        | Paren(_) => [],
        () => raise(HTyp.Tile.Void_PreOp),
        () => raise(HTyp.Tile.Void_PostOp),
        fun
        | Arrow
        | OperatorHole => [],
      );

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

    let view_of_ztile = (ztile: ZTyp.ztile): zipper_view(unit) =>
      switch (ztile) {
      | Operand(ParenZ_body({prefix, suffix, _})) =>
        let (l, r) = Text.of_Paren;
        let prefix = List.map(text_of_tile, prefix) @ [l];
        let suffix = [r, ...List.map(text_of_tile, suffix)];
        ZList.mk(~prefix, ~z=(), ~suffix, ());
      | PreOp () => raise(ZTyp.Void_ZPreOp)
      | PostOp(AnnZ_ann(_, {prefix, suffix, _})) =>
        let (l, r) = failwith("todo");
        let prefix = List.map(Pat.text_of_tile, prefix) @ [l];
        let suffix = [r, ...List.map(Pat.text_of_tile, suffix)];
        ZList.mk(~prefix, ~z=(), ~suffix, ());
      | BinOp () => raise(ZTyp.Void_ZBinOp)
      };

    let view_of_normal = (~font_metrics, (two_step, path), zipper) => {
      let `Typ(zipper) = ZPath.Typ.unzip(two_step, zipper);
      Typ.view_of_normal(~font_metrics, path, zipper);
    };

    let view_of_decorated_selection =
        (~font_metrics, (two_step, selection), zipper) => {
      let `Typ(zipper) = ZPath.Typ.unzip(two_step, zipper);
      Typ.view_of_decorated_selection(~font_metrics, selection, zipper);
    };

    let view_of_decorated_selection_tile =
        (~font_metrics as _, ~select as _, _, _) =>
      failwith("todo");
  };
  include Common(HTyp.Tile, ZTyp, V);
}
and Pat: PAT = {
  module V = {
    open HPat.Tile;

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
        | Operand(ParenZ_body(_)) => raise(ZPath.Unzip_rezip_changes_sort)
        | PreOp () => raise(ZTyp.Void_ZPreOp)
        | PostOp(AnnZ_ann(_)) => 1 + space + Typ.offset(path, ty)
        | BinOp () => raise(ZTyp.Void_ZBinOp)
        }
      | `Pat(p, unzipped) =>
        switch (Option.get(unzipped)) {
        | Operand(ParenZ_body(_)) => 1 + space + Pat.offset(path, p)
        | PreOp(LamZ_pat(_)) => raise(ZPath.Unzip_rezip_changes_sort)
        | PostOp () => raise(ZPat.Void_ZPostOp)
        | BinOp () => raise(ZPat.Void_ZBinOp)
        }
      };

    let text_of_tile = Text.Pat.view_of_tile;

    let is_operand_hole =
      fun
      | OperandHole => true
      | _ => false;
    let is_operator_hole =
      fun
      | OperatorHole => true;
    let open_children_of_tile =
      Tile.map(
        fun
        | OperandHole
        | Var(_) => []
        | Paren(body) => [(1 + space, Pat.length(body))],
        () => raise(HPat.Tile.Void_PreOp),
        fun
        | Ann(_) => [],
        fun
        | OperatorHole => [],
      );
    let closed_children_of_tile =
      Tile.map(
        fun
        | OperandHole
        | Var(_)
        | Paren(_) => [],
        () => raise(HPat.Tile.Void_PreOp),
        fun
        | Ann(_, ann) => [(1 + space, Typ.length(ann))],
        fun
        | OperatorHole => [],
      );

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

    let view_of_ztile = (ztile: ZPat.ztile): zipper_view(unit) => {
      switch (ztile) {
      | Operand(ParenZ_body({prefix, suffix, _})) =>
        let (l, r) = Text.of_Paren;
        let prefix = List.map(text_of_tile, prefix) @ [l];
        let suffix = [r, ...List.map(text_of_tile, suffix)];
        ZList.mk(~prefix, ~z=(), ~suffix, ());
      | PreOp(LamZ_pat(_, {prefix, suffix, _})) =>
        let (l, r) = Text.of_Lam;
        let prefix = List.map(Exp.text_of_tile, prefix) @ [l];
        let suffix = [r, ...List.map(Exp.text_of_tile, suffix)];
        ZList.mk(~prefix, ~z=(), ~suffix, ());
      | PostOp () => raise(ZPat.Void_ZPostOp)
      | BinOp () => raise(ZPat.Void_ZBinOp)
      };
    };

    let view_of_normal = (~font_metrics, (two_step, path), zipper) =>
      switch (ZPath.Pat.unzip(two_step, zipper)) {
      | `Typ(zipper) => Typ.view_of_normal(~font_metrics, path, zipper)
      | `Pat(zipper) => Pat.view_of_normal(~font_metrics, path, zipper)
      };

    let view_of_decorated_selection =
        (~font_metrics, (two_step, selection), zipper) =>
      switch (ZPath.Pat.unzip(two_step, zipper)) {
      | `Typ(zipper) =>
        Typ.view_of_decorated_selection(~font_metrics, selection, zipper)
      | `Pat(zipper) =>
        Pat.view_of_decorated_selection(~font_metrics, selection, zipper)
      };

    let view_of_decorated_selection_tile =
        (~font_metrics as _, ~select as _, _, _) =>
      failwith("todo");
  };
  include Common(HPat.Tile, ZPat, V);

  module M = {
    let length = length;
    let get_hole_status = HPat.get_hole_status;
    let inner_err_holes = (p: HPat.t) => {
      let shift = n => List.map(PairUtil.map_fst((+)(n + space)));
      switch (HPat.root(p)) {
      | Operand(OperandHole | Var(_)) => []
      | Operand(Paren(body)) => shift(1, Pat.err_holes(body))
      | PreOp(((), _)) => raise(HPat.Tile.Void_PreOp)
      | PostOp((subj, Ann(_))) => Pat.err_holes(subj)
      | BinOp((l, OperatorHole as binop, r)) =>
        let l_holes = Pat.err_holes(l);
        let r_holes =
          shift(
            length(l) + space + V.length_of_tile(BinOp(binop)),
            Pat.err_holes(r),
          );
        l_holes @ r_holes;
      };
    };
    let inner_err_holes_z = ((steps, j): ZPath.t, p: HPat.t) => {
      let shift = n => List.map(PairUtil.map_fst((+)(n + space)));
      switch (steps) {
      | [] =>
        switch (HPat.root(p)) {
        | Operand(OperandHole | Var(_)) => []
        | Operand(Paren(body)) => shift(1, Pat.err_holes(body))
        | PreOp(((), _)) => raise(HPat.Tile.Void_PreOp)
        | PostOp((subj, Ann(_))) =>
          j == List.length(subj)
            ? Pat.err_holes(subj) : Pat.err_holes_z(([], j), subj)
        | BinOp((l, OperatorHole as binop, r)) =>
          let n = List.length(l);
          let (err_holes_l, err_holes_r) =
            if (j < n) {
              (Pat.err_holes_z(([], j)), Pat.err_holes);
            } else if (j > n) {
              (Pat.err_holes, Pat.err_holes_z(([], j - (n + 1))));
            } else {
              (Pat.err_holes, Pat.err_holes);
            };
          err_holes_l(l)
          @ shift(
              length(l) + space + V.length_of_tile(BinOp(binop)),
              err_holes_r(r),
            );
        }
      | [(tile_step, child_step) as two_step, ...steps] =>
        switch (HPat.root(p)) {
        | Operand(OperandHole | Var(_)) => raise(ZPath.Out_of_sync)
        | Operand(Paren(body)) =>
          shift(1, Pat.err_holes_z((steps, j), body))
        | PreOp(((), _)) => raise(HPat.Tile.Void_PreOp)
        | PostOp((subj, Ann(_))) =>
          let in_postop = tile_step == List.length(subj);
          in_postop
            ? Pat.err_holes(subj)
            : Pat.err_holes_z(([two_step, ...steps], j), subj);
        | BinOp((l, OperatorHole as binop, r)) =>
          let n = List.length(l);
          let (err_holes_l, err_holes_r) =
            if (tile_step < n) {
              (Pat.err_holes_z(([two_step, ...steps], j)), Pat.err_holes);
            } else if (tile_step > n) {
              (
                Pat.err_holes,
                Pat.err_holes_z((
                  [(tile_step - (n + 1), child_step), ...steps],
                  j,
                )),
              );
            } else {
              raise(ZPath.Out_of_sync);
            };
          err_holes_l(l)
          @ shift(
              length(l) + space + V.length_of_tile(BinOp(binop)),
              err_holes_r(r),
            );
        }
      };
    };
  };
  include ErrHole(HPat.Tile, M);
}
and Exp: EXP = {
  module V = {
    open HExp.Tile;

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
        | Operand(ParenZ_body(_)) => raise(ZPath.Unzip_rezip_changes_sort)
        | PreOp(LamZ_pat(_)) => 1 + space + Pat.offset(path, p)
        | PostOp () => raise(ZPat.Void_ZPostOp)
        | BinOp () => raise(ZPat.Void_ZBinOp)
        }
      | `Exp(e, unzipped) =>
        switch (Option.get(unzipped)) {
        | Operand(ParenZ_body(_))
        | PostOp(ApZ_arg(_)) => 1 + space + Exp.offset(path, e)
        | PreOp () => raise(ZExp.Void_ZPreOp)
        | BinOp () => raise(ZExp.Void_ZBinOp)
        }
      };

    let text_of_tile = Text.Exp.view_of_tile;

    let is_operand_hole =
      fun
      | OperandHole => true
      | _ => false;
    let is_operator_hole =
      fun
      | OperatorHole => true
      | _ => false;
    let open_children_of_tile =
      Tile.map(
        fun
        | OperandHole
        | Var(_)
        | Num(_) => []
        | Paren(body) => [(1 + space, Exp.length(body))],
        fun
        | Lam(_) => [],
        fun
        | Ap(_, arg) => [(1 + space, Exp.length(arg))],
        fun
        | Plus(_)
        | OperatorHole => [],
      );
    let closed_children_of_tile =
      Tile.map(
        fun
        | OperandHole
        | Var(_)
        | Num(_)
        | Paren(_) => [],
        fun
        | Lam(_, p) => [(1 + space, Pat.length(p))],
        fun
        | Ap(_) => [],
        fun
        | Plus(_)
        | OperatorHole => [],
      );

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

    let view_of_ztile = (ztile: ZExp.ztile): zipper_view(unit) => {
      switch (ztile) {
      | Operand(ParenZ_body({prefix, suffix, _}))
      | PostOp(ApZ_arg(_, {prefix, suffix, _})) =>
        let (l, r) = Text.of_Paren;
        let prefix = List.map(text_of_tile, prefix) @ [l];
        let suffix = [r, ...List.map(text_of_tile, suffix)];
        ZList.mk(~prefix, ~z=(), ~suffix, ());
      | PreOp () => raise(ZExp.Void_ZPreOp)
      | BinOp () => raise(ZExp.Void_ZBinOp)
      };
    };

    let view_of_normal = (~font_metrics, (two_step, path), zipper) =>
      switch (ZPath.Exp.unzip(two_step, zipper)) {
      | `Pat(zipper) => Pat.view_of_normal(~font_metrics, path, zipper)
      | `Exp(zipper) => Exp.view_of_normal(~font_metrics, path, zipper)
      };

    let view_of_decorated_selection =
        (~font_metrics, (two_step, selection), zipper) =>
      switch (ZPath.Exp.unzip(two_step, zipper)) {
      | `Pat(zipper) =>
        Pat.view_of_decorated_selection(~font_metrics, selection, zipper)
      | `Exp(zipper) =>
        Exp.view_of_decorated_selection(~font_metrics, selection, zipper)
      };

    let view_of_decorated_selection_tile =
        (~font_metrics, ~select: Direction.t, (child_step, path), tile) =>
      switch (ZPath.Exp.unzip_tile(child_step, tile, ZExp.mk())) {
      | `Pat(_) => failwith("todo")
      | `Exp((e, unzipped) as zipper) =>
        switch (Option.get(unzipped)) {
        | Operand(ParenZ_body(_))
        | PostOp(ApZ_arg(_)) =>
          let (open_paren, close_paren) = Text.of_Paren;
          let (l, r) =
            switch (select) {
            | Left =>
              let ZList.{z: (_, selected, suffix), _} =
                Exp.view_of_decorated_selection(
                  ~font_metrics,
                  (([], 0), path),
                  zipper,
                );
              (selected, suffix);
            | Right =>
              let ZList.{z: (prefix, selected, _), _} =
                Exp.view_of_decorated_selection(
                  ~font_metrics,
                  (path, ([], List.length(e))),
                  zipper,
                );
              (prefix, selected);
            };
          ([open_paren, ...l], r @ [close_paren]);
        | PreOp () => raise(ZExp.Void_ZPreOp)
        | BinOp () => raise(ZExp.Void_ZBinOp)
        }
      };
  };
  include Common(HExp.Tile, ZExp, V);

  module M = {
    let length = length;
    let get_hole_status = HExp.get_hole_status;
    let inner_err_holes = (e: HExp.t) => {
      let shift = n => List.map(PairUtil.map_fst((+)(n + space)));
      switch (HExp.root(e)) {
      | Operand(OperandHole | Num(_) | Var(_)) => []
      | Operand(Paren(body)) => shift(1, Exp.err_holes(body))
      | PreOp((Lam(_, p) as preop, body)) =>
        let pat_holes = shift(1, Pat.err_holes(p));
        let body_holes =
          shift(V.length_of_tile(PreOp(preop)), Exp.err_holes(body));
        pat_holes @ body_holes;
      | PostOp((fn, Ap(_, arg))) =>
        let fn_holes = Exp.err_holes(fn);
        let arg_holes = shift(length(fn) + space + 1, Exp.err_holes(arg));
        fn_holes @ arg_holes;
      | BinOp((l, (OperatorHole | Plus(_)) as binop, r)) =>
        let l_holes = Exp.err_holes(l);
        let r_holes =
          shift(
            length(l) + space + V.length_of_tile(BinOp(binop)),
            Exp.err_holes(r),
          );
        l_holes @ r_holes;
      };
    };
    let inner_err_holes_z = ((steps, j): ZPath.t, e: HExp.t) => {
      let shift = n => List.map(PairUtil.map_fst((+)(n + space)));
      switch (steps) {
      | [] =>
        switch (HExp.root(e)) {
        | Operand(OperandHole | Num(_) | Var(_)) => []
        | Operand(Paren(body)) => shift(1, Exp.err_holes(body))
        | PreOp((Lam(_, p) as preop, body)) =>
          let p_holes = shift(1, Pat.err_holes(p));
          let body_holes = {
            let err_holes =
              j == 0 ? Exp.err_holes : Exp.err_holes_z(([], j - 1));
            shift(V.length_of_tile(PreOp(preop)), err_holes(body));
          };
          p_holes @ body_holes;
        | PostOp((fn, Ap(_, arg))) =>
          let fn_holes =
            j == List.length(fn)
              ? Exp.err_holes(fn) : Exp.err_holes_z(([], j), fn);
          let arg_holes = shift(length(fn), Exp.err_holes(arg));
          fn_holes @ arg_holes;
        | BinOp((l, (OperatorHole | Plus(_)) as binop, r)) =>
          let n = List.length(l);
          let (err_holes_l, err_holes_r) =
            if (j < n) {
              (Exp.err_holes_z(([], j)), Exp.err_holes);
            } else if (j > n) {
              (Exp.err_holes, Exp.err_holes_z(([], j - (n + 1))));
            } else {
              (Exp.err_holes, Exp.err_holes);
            };
          err_holes_l(l)
          @ shift(
              length(l) + space + V.length_of_tile(BinOp(binop)),
              err_holes_r(r),
            );
        }
      | [(tile_step, child_step) as two_step, ...steps] =>
        switch (HExp.root(e)) {
        | Operand(OperandHole | Num(_) | Var(_)) => raise(ZPath.Out_of_sync)
        | Operand(Paren(body)) =>
          shift(1, Exp.err_holes_z((steps, j), body))
        | PreOp((Lam(_, p) as preop, body)) =>
          let in_preop = tile_step == 0;
          let p_holes = {
            let err_holes =
              in_preop ? Pat.err_holes_z((steps, j)) : Pat.err_holes;
            shift(1, err_holes(p));
          };
          let body_holes = {
            let err_holes =
              in_preop
                ? Exp.err_holes
                : Exp.err_holes_z((
                    [(tile_step - 1, child_step), ...steps],
                    j,
                  ));
            shift(V.length_of_tile(PreOp(preop)), err_holes(body));
          };
          p_holes @ body_holes;
        | PostOp((fn, Ap(_, arg))) =>
          let in_postop = tile_step == List.length(fn);
          let fn_holes =
            in_postop
              ? Exp.err_holes(fn)
              : Exp.err_holes_z(([two_step, ...steps], j), fn);
          let arg_holes = {
            let err_holes =
              in_postop ? Exp.err_holes_z((steps, j)) : Exp.err_holes;
            shift(length(fn), err_holes(arg));
          };
          fn_holes @ arg_holes;
        | BinOp((l, (OperatorHole | Plus(_)) as binop, r)) =>
          let n = List.length(l);
          let (err_holes_l, err_holes_r) =
            if (tile_step < n) {
              (Exp.err_holes_z(([two_step, ...steps], j)), Exp.err_holes);
            } else if (tile_step > n) {
              (
                Exp.err_holes,
                Exp.err_holes_z((
                  [(tile_step - (n + 1), child_step), ...steps],
                  j,
                )),
              );
            } else {
              raise(ZPath.Out_of_sync);
            };
          err_holes_l(l)
          @ shift(
              length(l) + space + V.length_of_tile(BinOp(binop)),
              err_holes_r(r),
            );
        }
      };
    };
  };
  include ErrHole(HExp.Tile, M);
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
         Decoration.EmptyHole.view(~radii, ~inset=false),
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
  |> List.map(((origin, profile: Decoration.ErrHole.profile)) =>
       decoration_container(
         ~font_metrics,
         ~origin,
         ~length=profile.len,
         ~cls="err-hole",
         Decoration.ErrHole.view(profile),
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
  let zipper =
    switch (zipper) {
    | `Typ(zipper) => Typ.view(~font_metrics, mode, zipper)
    | `Pat(zipper) => Pat.view(~font_metrics, mode, zipper)
    | `Exp(zipper) => Exp.view(~font_metrics, mode, zipper)
    };
  Node.span(
    [Attr.id("code")],
    List.concat([empty_holes, err_holes, [zipper]]),
  );
};
