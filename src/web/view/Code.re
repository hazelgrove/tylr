open Virtual_dom.Vdom;
open Util;
open Core;

let decoration_container =
    (
      ~font_metrics: FontMetrics.t,
      ~origin: int,
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

  let normal_tiles = (_, _) => failwith("todo");
  let offset = _ => failwith("todo");
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
    let (_, holes) =
      e
      |> ListUtil.fold_left_map(
           (start, tile) => {
             let holes = empty_holes_tile(tile) |> List.map((+)(start));
             (start + length_of_tile(tile) + 1, holes);
           },
           0,
         );
    List.concat(holes);
  }
  and empty_holes_tile = (tile: HExp.Tile.t): list(int) => {
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
        | Operand(Paren(body)) =>
          err_holes_z((steps, j), body) |> shift(2)
        | PreOp((Lam(_, p) as preop, body)) =>
          let holes_p =
            tile_step == 0
              ? Pat.err_holes_z((steps, j), p) : Pat.err_holes(p);
          let holes_body =
            tile_step == 0
              ? err_holes(body)
              : {
                let two_step = PairUtil.map_fst((-)(1), two_step);
                err_holes_z(([two_step, ...steps], j), body);
              };
          let holes_p = shift(2, holes_p);
          let holes_body = shift(length_of_preop(preop) + 1, holes_body);
          holes_p @ holes_body;
        | PostOp((fn, Ap(_, arg))) =>
          let fn_len = List.length(fn);
          let holes_fn =
            tile_step == fn_len
              ? err_holes(fn) : err_holes_z(([two_step, ...steps], j), fn);
          let holes_arg =
            tile_step == fn_len
              ? err_holes_z((steps, j), arg) : err_holes(arg);
          let holes_arg = shift(fn_len + 1, holes_arg);
          holes_fn @ holes_arg;
        | Operand(OperandHole | Num(_) | Var(_))
        | BinOp((_, OperatorHole | Plus(_), _)) => raise(ZPath.Out_of_sync)
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
      {shape: `BinOp(is_hole), len, open_children, closed_children};
    };
  };

  let offset = (ztile: ZExp.ztile): int =>
    switch (ztile) {
    | Operand(ParenZ_body({prefix, _})) => length(prefix) + 3
    | PreOp(_) => raise(ZExp.Void_ZPreOp)
    | PostOp(ApZ_arg(_, {prefix, _})) => length(prefix) + 3
    | BinOp(_) => raise(ZExp.Void_ZBinOp)
    };

  let rec normal_tiles =
          ((steps, j): ZPath.t, e: HExp.t)
          : list((int, CodeDecoration.Tile.profile)) =>
    switch (steps) {
    | [] =>
      e
      |> ListUtil.fold_left_map(
           (start, tile) =>
             (
               start + length_of_tile(tile) + 1,
               (start, profile_of_tile(tile)),
             ),
           0,
         )
      |> snd
    | [two_step, ...steps] =>
      let (profiles, offset) =
        switch (ZPath.Exp.unzip(two_step, (e, None))) {
        | `Exp(e, unzipped) => (
            normal_tiles((steps, j), e),
            offset(Option.get(unzipped)),
          )
        | `Pat(p, unzipped) => (
            Pat.normal_tiles((steps, j), p),
            Pat.offset(Option.get(unzipped)),
          )
        };
      profiles |> List.map(((start, profile)) => (offset + start, profile));
    };
};

let empty_hole = ((r_x, r_y): (float, float)) =>
  Node.create_svg(
    "ellipse",
    AttrUtil.[
      cx(0.5),
      cy(0.5),
      rx(r_x),
      ry(r_y),
      Attr.classes(["empty-hole-ellipse"]),
      Attr.create("vector-effect", "non-scaling-stroke"),
    ],
    [],
  );

let view = (~font_metrics: FontMetrics.t, edit_state: EditState.t) => {
  let decoration_container = decoration_container(~font_metrics);
  let hole_radii = {
    let r = 3.5;
    (r /. font_metrics.col_width, r /. font_metrics.row_height);
  };
  let (mode, e) =
    switch (EditState.zip_up(edit_state)) {
    | (_, `Typ(_) | `Pat(_)) => failwith("expected expression at top level")
    | (mode, `Exp(e, _)) => (mode, e)
    };
  let text = CodeText.Exp.view(~attrs=[Attr.classes(["code-text"])], e);
  let empty_holes =
    Exp.empty_holes(e)
    |> List.map(origin =>
         decoration_container(
           ~origin,
           ~length=1,
           ~cls="empty-hole",
           [empty_hole(hole_radii)],
         )
       );
  let err_holes = {
    let holes =
      switch (mode) {
      | Normal(focus) => Exp.err_holes_z(focus, e)
      | Selecting(_) => Exp.err_holes(e)
      | Restructuring(_) => []
      };
    holes
    |> List.map(((origin, profile: CodeDecoration.ErrHole.profile)) =>
         decoration_container(
           ~origin,
           ~length=profile.len,
           ~cls="err-hole",
           CodeDecoration.ErrHole.view(profile),
         )
       );
  };
  let tiles = {
    let tiles =
      switch (mode) {
      | Normal(focus) => Exp.normal_tiles(focus, e)
      | _ => failwith("todo")
      };
    tiles
    |> List.map(((origin, profile: CodeDecoration.Tile.profile)) =>
         decoration_container(
           ~origin,
           ~length=profile.len,
           ~cls="tile",
           CodeDecoration.Tile.view(~sort=Exp, ~hole_radii, profile),
         )
       );
  };
  Node.span(
    [Attr.id("code")],
    List.concat([[text], tiles, empty_holes, err_holes]),
  );
};
