open Virtual_dom.Vdom;

module Tile = {
  type profile = {
    shape: [ | `Operand | `PreOp | `PostOp | `BinOp],
    len: int,
    open_children: list((int, int)),
    closed_children: list((int, int)),
  };

  let tip = 0.4;
  let child_border_thickness = 0.1;

  let tr_bl = (~child_border: option([ | `North | `South])=?, ()) =>
    SvgUtil.Path.(
      switch (child_border) {
      | None =>
        let diag = L_({dx: Float.neg(tip), dy: 0.5});
        [diag];
      | Some(d) =>
        let t = child_border_thickness /. 0.5;
        let dx = (1. -. t) *. tip;
        let diag = L_({dx: Float.neg(dx), dy: (1. -. t) *. 0.5});
        let border = H_({dx: Float.neg(1. -. dx)});
        switch (d) {
        | `North => [border, diag]
        | `South => [diag, border]
        };
      }
    );
  let bl_tr = (~child_border: option([ | `North | `South])=?, ()) =>
    SvgUtil.Path.reverse(tr_bl(~child_border?, ()));

  let tl_br = (~child_border: option([ | `North | `South])=?, ()) =>
    SvgUtil.Path.(
      switch (child_border) {
      | None =>
        let diag = L_({dx: tip, dy: 0.5});
        [diag];
      | Some(d) =>
        let t = child_border_thickness /. 0.5;
        let dx = (1. -. t) *. tip;
        let diag = L_({dx, dy: (1. -. t) *. 0.5});
        let border = H_({dx: 1. -. dx});
        switch (d) {
        | `North => [border, diag]
        | `South => [diag, border]
        };
      }
    );
  let br_tl = (~child_border: option([ | `North | `South])=?, ()) =>
    SvgUtil.Path.reverse(tl_br(~child_border?, ()));

  let view = (~attrs: list(Attr.t)=[], profile: profile): Node.t => {
    open SvgUtil.Path;
    let closed_child_paths =
      profile.closed_children
      |> List.map(((start, len)) =>
           List.concat([
             [M({x: Float.of_int(start), y: child_border_thickness})],
             tr_bl(~child_border=`North, ()),
             tl_br(~child_border=`South, ()),
             [H_({dx: Float.of_int(len)})],
             bl_tr(~child_border=`South, ()),
             br_tl(~child_border=`North, ()),
             [Z],
           ])
         );
    let outer_path = {
      let (left_tip, right_tip) =
        switch (profile.shape) {
        | `Operand => (br_tl() @ bl_tr(), tl_br() @ tr_bl())
        | `PreOp => (
            br_tl() @ bl_tr(),
            [H_({dx: tip}), ...tr_bl()]
            @ tl_br()
            @ [H_({dx: Float.neg(tip)})],
          )
        | `PostOp => (
            [H_({dx: Float.neg(tip)}), ...bl_tr()]
            @ br_tl()
            @ [H_({dx: tip})],
            tl_br() @ tr_bl(),
          )
        | `BinOp => (
            [H_({dx: Float.neg(tip)}), ...bl_tr()]
            @ br_tl()
            @ [H_({dx: tip})],
            [H_({dx: tip}), ...tr_bl()]
            @ tl_br()
            @ [H_({dx: Float.neg(tip)})],
          )
        };
      let left_and_top_sides =
        profile.open_children
        |> List.map(((start, len)) =>
             List.concat([
               [H({x: Float.of_int(start) -. 1. +. tip})],
               tr_bl(),
               tl_br(~child_border=`South, ()),
               [H_({dx: Float.of_int(len)})],
               bl_tr(~child_border=`South, ()),
               br_tl(),
             ])
           )
        |> List.cons([M({x: 0., y: 1.}), ...left_tip])
        |> List.concat;
      List.concat([
        left_and_top_sides,
        [H({x: Float.of_int(profile.len)}), ...right_tip],
        [Z],
      ]);
    };
    let path = outer_path @ List.concat(closed_child_paths);
    SvgUtil.Path.view(~attrs, path);
  };

  let shadow_filter = clss =>
    Node.create_svg(
      "filter",
      [Attr.id("outer-drop-shadow")],
      [
        Node.create_svg(
          "feDropShadow",
          [
            Attr.classes(["tile-decoration-drop-shadow", ...clss]),
            Attr.create("dx", "0.1"),
            Attr.create("dy", "0.04"),
            Attr.create("stdDeviation", "0"),
          ],
          [],
        ),
      ],
    );
};
