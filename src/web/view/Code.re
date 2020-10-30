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

module Typ = {
  let rec length = (ty: HTyp.t) =>
    ty
    |> List.map(
         Tile.map(
           length_of_operand,
           length_of_preop,
           length_of_postop,
           length_of_binop,
         ),
       )
    |> List.map((+)(1))
    |> List.fold_left((+), -1)
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

  let view_of_ztile = _ => failwith("todo");
  let view = (~font_metrics as _, _, _) => failwith("todo");
};

module Pat = {
  let rec length = (p: HPat.t) =>
    p
    |> List.map(
         Tile.map(
           length_of_operand,
           length_of_preop,
           length_of_postop,
           length_of_binop,
         ),
       )
    |> List.map((+)(1))
    |> List.fold_left((+), -1)
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
    | Ann(_, ann) => 2 + Typ.length(ann)
  and length_of_binop: HPat.Tile.binop => int =
    fun
    | OperatorHole => 1;

  let empty_holes = _ => failwith("todo");
  let err_holes = _ => failwith("todo");
  let err_holes_z = (_, _) => failwith("todo");

  let view_of_ztile = _ => failwith("todo");
  let view_of_normal = (~font_metrics as _, _, _) => failwith("todo");
  let view = (~font_metrics as _, _, _) => failwith("todo");

  let normal_tiles = (_, _) => failwith("todo");
  let offset_of_ztile = _ => failwith("todo");
};

module Exp = {
  let rec length = (e: HExp.t) =>
    e
    |> List.map(length_of_tile)
    |> List.map((+)(1))
    |> List.fold_left((+), -1)
  and length_of_tile = tile =>
    Tile.map(
      length_of_operand,
      length_of_preop,
      length_of_postop,
      length_of_binop,
      tile,
    )
  and length_of_operand =
    fun
    | OperandHole => 1
    | Var(_, x) => String.length(x)
    | Num(_, n) => String.length(string_of_int(n))
    | Paren(body) => 4 + length(body)
  and length_of_preop =
    fun
    | Lam(_, p) => 4 + Pat.length(p)
  and length_of_postop =
    fun
    | Ap(_, arg) => 4 + length(arg)
  and length_of_binop =
    fun
    | OperatorHole
    | Plus(_) => 1;

  let rec empty_holes = (e: HExp.t): list(int) => {
    let of_tile = (tile: HExp.Tile.t): list(int) => {
      let shift = List.map((+)(2));
      switch (tile) {
      | Operand(OperandHole) => [0]
      | Operand(Num(_) | Var(_)) => []
      | Operand(Paren(body)) => shift(empty_holes(body))
      | PreOp(Lam(_, p)) => shift(Pat.empty_holes(p))
      | PostOp(Ap(_, arg)) => shift(empty_holes(arg))
      | BinOp(OperatorHole) => [0]
      | BinOp(Plus(_)) => []
      };
    };
    let (_, holes) =
      e
      |> ListUtil.fold_left_map(
           (start, tile) => {
             let origins = of_tile(tile) |> List.map((+)(start));
             (start + length_of_tile(tile) + 1, origins);
           },
           0,
         );
    List.concat(holes);
  };

  let rec err_holes =
          (~expanded=false, e: HExp.t)
          : list((int, CodeDecoration.ErrHole.profile)) => {
    let shift = n => List.map(PairUtil.map_fst((+)(n)));
    switch (HExp.root(e)) {
    | Operand(operand) =>
      switch (HExp.get_hole_status_operand(operand)) {
      | NotInHole => []
      | InHole =>
        // TODO missing inner holes
        let len = length_of_operand(operand);
        [(0, {expanded, len})];
      }
    | PreOp((preop, r)) =>
      let holes_preop =
        switch (preop) {
        | Lam(status, p) =>
          let outer_hole =
            switch (status) {
            | NotInHole => []
            | InHole =>
              let len = length_of_preop(preop) + length(r);
              [(0, CodeDecoration.ErrHole.{expanded, len})];
            };
          let inner_holes = Pat.err_holes(p) |> shift(2);
          outer_hole @ inner_holes;
        };
      let holes_r =
        err_holes(~expanded, r) |> shift(length_of_preop(preop));
      holes_preop @ holes_r;
    | PostOp((l, postop)) =>
      let holes_l = err_holes(~expanded, l);
      let holes_postop =
        switch (postop) {
        | Ap(status, arg) =>
          let outer_hole =
            switch (status) {
            | NotInHole => []
            | InHole =>
              let len = length(l) + length_of_postop(postop);
              [(0, CodeDecoration.ErrHole.{expanded, len})];
            };
          let inner_holes = shift(2, err_holes(arg));
          outer_hole @ inner_holes;
        };
      holes_l @ holes_postop;
    | BinOp((l, binop, r)) =>
      let holes_l = err_holes(~expanded, l);
      let holes_binop =
        switch (binop) {
        | OperatorHole
        | Plus(NotInHole) => []
        | Plus(InHole) =>
          let len = length(l) + length_of_binop(binop);
          [(0, CodeDecoration.ErrHole.{expanded, len})];
        };
      let holes_r = err_holes(~expanded, r);
      holes_l @ holes_binop @ holes_r;
    };
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
          let holes_body = shift(length_of_preop(preop) + 1, holes_body);
          holes_p @ holes_body;
        | PostOp((fn, Ap(_, arg))) =>
          let in_postop = tile_step == List.length(fn);
          let holes_fn =
            in_postop
              ? err_holes(fn) : err_holes_z(([two_step, ...steps], j), fn);
          let holes_arg =
            in_postop ? err_holes_z((steps, j), arg) : err_holes(arg);
          let holes_arg = shift(length(fn) + 1, holes_arg);
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
            shift(length(l) + 1 + length_of_binop(binop) + 1, holes_r);
          holes_l @ holes_r;
        };
      };
      outer_hole @ inner_holes;
    };

  let profile_of_tile = (tile: HExp.Tile.t): CodeDecoration.Tile.profile => {
    switch (tile) {
    | Operand(operand) =>
      let (open_children, closed_children, len, is_hole) =
        switch (operand) {
        | OperandHole => ([], [], 1, true)
        | Var(_)
        | Num(_) => ([], [], length_of_operand(operand), false)
        | Paren(body) =>
          let body_len = length(body);
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
          let arg_len = length(arg);
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

  let view_of_decorated_tile =
      (~font_metrics: FontMetrics.t, tile: HExp.Tile.t): Node.t => {
    let hole_radii = hole_radii(~font_metrics);
    let text = CodeText.Exp.view_of_tile(tile);
    let decoration = {
      let profile = profile_of_tile(tile);
      decoration_container(
        ~font_metrics,
        ~length=profile.len,
        ~cls="tile",
        CodeDecoration.Tile.view(~sort=Exp, ~hole_radii, profile),
      );
    };
    Node.span([Attr.classes(["decorated-tile"])], [text, decoration]);
  };

  let view_of_decorated_open_child =
      (~font_metrics: FontMetrics.t, e: HExp.t): Node.t => {
    let text = CodeText.Exp.view(e);
    let contour = {
      let length = length(e);
      decoration_container(
        ~font_metrics,
        ~length,
        ~cls="open-child",
        CodeDecoration.OpenChild.view(~sort=Exp, length),
      );
    };
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
    Node.span(
      [Attr.classes(["decorated-open-child"])],
      [text, contour, ...inset_empty_holes],
    );
  };

  let view_of_decorated_term =
      (~font_metrics: FontMetrics.t, root: HExp.root): Node.t => {
    let view_of_decorated_tile = view_of_decorated_tile(~font_metrics);
    let view_of_decorated_open_child =
      view_of_decorated_open_child(~font_metrics);
    let vs =
      switch (root) {
      | Operand(operand) => [view_of_decorated_tile(Operand(operand))]
      | PreOp((preop, r)) => [
          view_of_decorated_tile(PreOp(preop)),
          view_of_decorated_open_child(r),
        ]
      | PostOp((l, postop)) => [
          view_of_decorated_open_child(l),
          view_of_decorated_tile(PostOp(postop)),
        ]
      | BinOp((l, binop, r)) => [
          view_of_decorated_open_child(l),
          view_of_decorated_tile(BinOp(binop)),
          view_of_decorated_open_child(r),
        ]
      };
    Node.span([Attr.classes(["decorated-term"])], CodeText.space(vs));
  };

  let view_of_ztile = (ztile: ZExp.ztile) => {
    switch (ztile) {
    | Operand(ParenZ_body({prefix, suffix, _})) =>
      let prefix = List.map(CodeText.Exp.view_of_tile, prefix);
      let (l, r) = CodeText.of_Paren;
      let suffix = List.map(CodeText.Exp.view_of_tile, suffix);
      (
        Node.span([], CodeText.space(prefix @ [l])),
        Node.span([], CodeText.space([r, ...suffix])),
      );
    | PreOp(_) => raise(ZExp.Void_ZPreOp)
    | PostOp(ApZ_arg(_, {prefix, suffix, _})) =>
      let prefix = List.map(CodeText.Exp.view_of_tile, prefix);
      let (l, r) = CodeText.of_Paren;
      let suffix = List.map(CodeText.Exp.view_of_tile, suffix);
      (
        Node.span([], CodeText.space(prefix @ [l])),
        Node.span([], CodeText.space([r, ...suffix])),
      );
    | BinOp(_) => raise(ZExp.Void_ZBinOp)
    };
  };

  let view_of_normal =
      (~font_metrics: FontMetrics.t, (steps, j): ZPath.t, e: HExp.t): Node.t => {
    let view_of_decorated_term = view_of_decorated_term(~font_metrics);
    let rec go = (steps, e) =>
      switch (steps) {
      | [] =>
        let caret = {
          let (prefix, _) = ListUtil.split_n(j, e);
          let len = length(prefix);
          CodeDecoration.Caret.view(~font_metrics, len, []);
        };
        let code =
          if (j == List.length(e)) {
            [CodeText.Exp.view(e)];
          } else {
            let ZList.{prefix, z, suffix} = HExp.nth_root(j, e);
            let prefix = List.map(CodeText.Exp.view_of_tile, prefix);
            let zroot = view_of_decorated_term(z);
            let suffix = List.map(CodeText.Exp.view_of_tile, suffix);
            CodeText.space(prefix @ [zroot, ...suffix]);
          };
        Node.span([Attr.classes(["zipped"])], [caret, ...code]);
      | [two_step, ...steps] =>
        switch (ZPath.Exp.unzip(two_step, (e, None))) {
        | `Pat(p, unzipped) =>
          let (l, r) = Pat.view_of_ztile(Option.get(unzipped));
          let p = Pat.view_of_normal(~font_metrics, (steps, j), p);
          Node.span([], CodeText.space([l, p, r]));
        | `Exp(e, unzipped) =>
          let (l, r) = view_of_ztile(Option.get(unzipped));
          let e = go(steps, e);
          Node.span([], CodeText.space([l, e, r]));
        }
      };
    go(steps, e);
  };

  let view =
      (~font_metrics: FontMetrics.t, mode: EditState.Mode.t, e: HExp.t)
      : Node.t =>
    switch (mode) {
    | Normal(focus) => view_of_normal(~font_metrics, focus, e)
    | Selecting(_)
    | Restructuring(_) => failwith("todo")
    };

  let offset_of_ztile = (ztile: ZExp.ztile): int =>
    switch (ztile) {
    | Operand(ParenZ_body({prefix, _})) => length(prefix) + 3
    | PreOp(_) => raise(ZExp.Void_ZPreOp)
    | PostOp(ApZ_arg(_, {prefix, _})) => length(prefix) + 3
    | BinOp(_) => raise(ZExp.Void_ZBinOp)
    };

  let rec normal_tiles =
          ((steps, j): ZPath.t, e: HExp.t)
          : (int, list((int, CodeDecoration.Tile.profile))) =>
    switch (steps) {
    | [] =>
      let tile_profiles =
        e
        |> ListUtil.fold_left_map(
             (start, tile) =>
               (
                 start + length_of_tile(tile) + 1,
                 (start, profile_of_tile(tile)),
               ),
             0,
           )
        |> snd;
      let caret = {
        let (prefix, _) = ListUtil.split_n(j, e);
        length(prefix);
      };
      (caret, tile_profiles);
    | [two_step, ...steps] =>
      let ((caret, profiles), offset) =
        switch (ZPath.Exp.unzip(two_step, (e, None))) {
        | `Exp(e, unzipped) => (
            normal_tiles((steps, j), e),
            offset_of_ztile(Option.get(unzipped)),
          )
        | `Pat(p, unzipped) => (
            Pat.normal_tiles((steps, j), p),
            Pat.offset_of_ztile(Option.get(unzipped)),
          )
        };
      let caret = offset + caret;
      let profiles =
        profiles
        |> List.map(((start, profile)) => (offset + start, profile));
      (caret, profiles);
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
    | Some((l, r)) => ([l], [r])
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
