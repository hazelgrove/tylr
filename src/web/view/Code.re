open Virtual_dom.Vdom;
open Util;
open Core;

type zipper_view('z) = ZList.t('z, Node.t);

module type COMMON = {
  module T: Tile.S;
  module Z: ZTile.S with module T := T;

  let view_of_ztile: Z.ztile => zipper_view(unit);
  let view_of_normal:
    (~font_metrics: FontMetrics.t, ZPath.t, Z.zipper) => zipper_view(Node.t);
  let view_of_decorated_selection:
    (~font_metrics: FontMetrics.t, ZPath.ordered_selection, Z.zipper) =>
    zipper_view((list(Node.t), list(Node.t), list(Node.t)));
  let view_of_decorated_partition:
    (~font_metrics: FontMetrics.t, ~partition: Direction.t, ZPath.t, T.s) =>
    (list(Node.t), list(Node.t));
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
         Txt: Text.COMMON with module T := T,
         M: Measured.COMMON with module T := T,
         V: {
           let view_of_ztile: Z.ztile => zipper_view(unit);
           let view_of_normal:
             (
               ~font_metrics: FontMetrics.t,
               (ZPath.two_step, ZPath.t),
               Z.zipper
             ) =>
             zipper_view(Node.t);
           let view_of_decorated_partition:
             (
               ~font_metrics: FontMetrics.t,
               ~partition: Direction.t,
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
  let view_of_ztile = V.view_of_ztile;

  let view_of_decorated_tile =
      (~font_metrics: FontMetrics.t, tile: T.t): Node.t => {
    let text = Txt.view_of_tile(tile);
    let decoration = {
      let profile = M.profile_of_tile(tile);
      Decoration.container(
        ~font_metrics,
        ~length=profile.len,
        ~cls="tile",
        Decoration.Tile.view(~sort=T.sort, ~font_metrics, profile),
      );
    };
    Node.span([Attr.classes(["decorated-tile"])], [text, decoration]);
  };

  let view_of_decorated_open_child =
      (~font_metrics: FontMetrics.t, ~side: Direction.t, ts: T.s): Node.t => {
    let text = Txt.view(ts);
    let contour = {
      let length = M.length(ts);
      Decoration.container(
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
            Decoration.container(
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
    | Some(ztile) => view_of_ztile(ztile)
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
          let len = M.length(prefix);
          Decoration.Caret.view(~font_metrics, len, []);
        };
        let code = {
          let k = j == List.length(ts) ? j - 1 : j;
          let ZList.{prefix, z, suffix} = Ts.nth_root(k, ts);
          let prefix = List.map(Txt.view_of_tile, prefix);
          let zroot = view_of_decorated_term(z);
          let suffix = List.map(Txt.view_of_tile, suffix);
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
          let prefix = List.map(Txt.view_of_tile, prefix);
          let selected = List.map(view_of_decorated_tile, selected);
          let suffix = List.map(Txt.view_of_tile, suffix);
          (prefix, selected, suffix);
        | ([], [(tile_step_r, child_step_r), ...steps_r]) =>
          let (prefix, tile, suffix) = ListUtil.split_nth(tile_step_r, ts);
          let (prefix, selected) = ListUtil.split_n(j_l, prefix);
          let prefix = List.map(Txt.view_of_tile, prefix);
          let suffix = List.map(Txt.view_of_tile, suffix);
          let selected = List.map(view_of_decorated_tile, selected);
          let (inner_selected, inner_unselected) =
            V.view_of_decorated_partition(
              ~font_metrics,
              ~partition=Left,
              (child_step_r, (steps_r, j_r)),
              tile,
            );
          (prefix, selected @ inner_selected, inner_unselected @ suffix);
        | ([(tile_step_l, child_step_l), ...steps_l], []) =>
          let (prefix, tile, suffix) = ListUtil.split_nth(tile_step_l, ts);
          let (selected, suffix) =
            ListUtil.split_n(j_r - List.length(prefix) - 1, suffix);
          let prefix = List.map(Txt.view_of_tile, prefix);
          let suffix = List.map(Txt.view_of_tile, suffix);
          let selected = List.map(view_of_decorated_tile, selected);
          let (inner_unselected, inner_selected) =
            V.view_of_decorated_partition(
              ~font_metrics,
              ~partition=Right,
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
          let prefix = List.map(Txt.view_of_tile, prefix);
          let (unselected_l, selected_l) =
            V.view_of_decorated_partition(
              ~font_metrics,
              ~partition=Right,
              (child_step_l, (steps_l, j_l)),
              tile_l,
            );
          let mid = List.map(view_of_decorated_tile, mid);
          let (selected_r, unselected_r) =
            V.view_of_decorated_partition(
              ~font_metrics,
              ~partition=Left,
              (child_step_r, (steps_r, j_r)),
              tile_r,
            );
          let suffix = List.map(Txt.view_of_tile, suffix);
          (
            prefix @ unselected_l,
            selected_l @ mid @ selected_r,
            unselected_r @ suffix,
          );
        };
      ZList.mk(~prefix, ~z, ~suffix, ());
    };
  };
  let view_of_decorated_partition =
      (~font_metrics, ~partition: Direction.t, path, ts) =>
    switch (partition) {
    | Left =>
      let ZList.{z: (_, selected, suffix), _} =
        view_of_decorated_selection(
          ~font_metrics,
          (([], 0), path),
          (ts, None),
        );
      (selected, suffix);
    | Right =>
      let ZList.{z: (prefix, selected, _), _} =
        view_of_decorated_selection(
          ~font_metrics,
          (path, ([], List.length(ts))),
          (ts, None),
        );
      (prefix, selected);
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
      let (offset_l, offset_r) = (M.offset(l, ts), M.offset(r, ts));
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
    let selection_len = M.offset(r, ts) - M.offset(l, ts);
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
      let offset = M.offset(target, ts);
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

module type TYP = COMMON with module T := HTyp.Tile and module Z := ZTyp;
module type PAT = COMMON with module T := HPat.Tile and module Z := ZPat;
module type EXP = COMMON with module T := HExp.Tile and module Z := ZExp;

module rec Typ: TYP = {
  module V = {
    open HTyp.Tile;

    let view_of_ztile = (ztile: ZTyp.ztile): zipper_view(unit) =>
      switch (ztile) {
      | Operand(ParenZ_body({prefix, suffix, _})) =>
        let (l, r) = Text.of_Paren;
        let prefix = List.map(Text.Typ.view_of_tile, prefix) @ [l];
        let suffix = [r, ...List.map(Text.Typ.view_of_tile, suffix)];
        ZList.mk(~prefix, ~z=(), ~suffix, ());
      | PreOp () => raise(ZTyp.Void_ZPreOp)
      | PostOp(AnnZ_ann(_, {prefix, suffix, _})) =>
        let (l, r) = Text.of_Ann;
        let prefix = List.map(Text.Pat.view_of_tile, prefix) @ [l];
        let suffix = [r, ...List.map(Text.Pat.view_of_tile, suffix)];
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

    let view_of_decorated_partition =
        (~font_metrics, ~partition: Direction.t, (child_step, path), tile) => {
      let `Typ(ty, unzipped) =
        ZPath.Typ.unzip_tile(child_step, tile, ZTyp.mk());
      let ZList.{prefix, z: (), suffix} =
        view_of_ztile(Option.get(unzipped));
      let (l, r) =
        Typ.view_of_decorated_partition(~font_metrics, ~partition, path, ty);
      (prefix @ l, r @ suffix);
    };
  };
  include Common(HTyp.Tile, ZTyp, Text.Typ, Measured.Typ, V);
}
and Pat: PAT = {
  module V = {
    open HPat.Tile;

    let view_of_ztile = (ztile: ZPat.ztile): zipper_view(unit) => {
      switch (ztile) {
      | Operand(ParenZ_body({prefix, suffix, _})) =>
        let (l, r) = Text.of_Paren;
        let prefix = List.map(Text.Pat.view_of_tile, prefix) @ [l];
        let suffix = [r, ...List.map(Text.Pat.view_of_tile, suffix)];
        ZList.mk(~prefix, ~z=(), ~suffix, ());
      | PreOp(LamZ_pat(_, {prefix, suffix, _})) =>
        let (l, r) = Text.of_Lam;
        let prefix = List.map(Text.Exp.view_of_tile, prefix) @ [l];
        let suffix = [r, ...List.map(Text.Exp.view_of_tile, suffix)];
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

    let view_of_decorated_partition =
        (~font_metrics, ~partition, (child_step, path), tile) => {
      let (ZList.{prefix, z: (), suffix}, (l, r)) =
        switch (ZPath.Pat.unzip_tile(child_step, tile, ZPat.mk())) {
        | `Typ(ty, unzipped) => (
            Typ.view_of_ztile(Option.get(unzipped)),
            Typ.view_of_decorated_partition(
              ~font_metrics,
              ~partition,
              path,
              ty,
            ),
          )
        | `Pat(p, unzipped) => (
            Pat.view_of_ztile(Option.get(unzipped)),
            Pat.view_of_decorated_partition(
              ~font_metrics,
              ~partition,
              path,
              p,
            ),
          )
        };
      (prefix @ l, r @ suffix);
    };
  };
  include Common(HPat.Tile, ZPat, Text.Pat, Measured.Pat, V);
}
and Exp: EXP = {
  module V = {
    open HExp.Tile;

    let view_of_ztile = (ztile: ZExp.ztile): zipper_view(unit) => {
      switch (ztile) {
      | Operand(ParenZ_body({prefix, suffix, _}))
      | PostOp(ApZ_arg(_, {prefix, suffix, _})) =>
        let (l, r) = Text.of_Paren;
        let prefix = List.map(Text.Exp.view_of_tile, prefix) @ [l];
        let suffix = [r, ...List.map(Text.Exp.view_of_tile, suffix)];
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

    let view_of_decorated_partition =
        (~font_metrics, ~partition: Direction.t, (child_step, path), tile) => {
      let (ZList.{prefix, z: (), suffix}, (l, r)) =
        switch (ZPath.Exp.unzip_tile(child_step, tile, ZExp.mk())) {
        | `Pat(p, unzipped) => (
            Pat.view_of_ztile(Option.get(unzipped)),
            Pat.view_of_decorated_partition(
              ~font_metrics,
              ~partition,
              path,
              p,
            ),
          )
        | `Exp(e, unzipped) => (
            Exp.view_of_ztile(Option.get(unzipped)),
            Exp.view_of_decorated_partition(
              ~font_metrics,
              ~partition,
              path,
              e,
            ),
          )
        };
      (prefix @ l, r @ suffix);
    };
  };
  include Common(HExp.Tile, ZExp, Text.Exp, Measured.Exp, V);
};

let empty_holes = (~font_metrics: FontMetrics.t, e: HExp.t): list(Node.t) => {
  Measured.Exp.empty_holes(e)
  |> List.map(origin =>
       Decoration.container(
         ~font_metrics,
         ~origin,
         ~length=1,
         ~cls="empty-hole",
         Decoration.EmptyHole.view(~font_metrics, ~inset=false),
       )
     );
};

let err_holes =
    (~font_metrics, mode: EditState.Mode.t, e: HExp.t): list(Node.t) => {
  let profiles =
    switch (mode) {
    | Normal(focus) => Measured.Exp.err_holes_z(focus, e)
    | Selecting(_) => Measured.Exp.err_holes(e)
    | Restructuring(_) => []
    };
  profiles
  |> List.map(((origin, profile: Decoration.ErrHole.profile)) =>
       Decoration.container(
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
