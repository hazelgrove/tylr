open Util;
open Core;
open Virtual_dom.Vdom;
open DecUtil;

[@deriving show]
type piece_shape = (Diag.tip_shape, Diag.tip_shape);

let piece_shape_of_nibs = ((l, r): Core.Nibs.t): piece_shape => (
  (l, 0),
  (r, 0),
);
module SelemDecProfile = {
  type t = {
    measurement: Measured.measurement,
    color: Color.t,
    shape: piece_shape,
    style: SelemStyle.t,
  };
};

let raised_shadow_filter = (~color: Color.t) => {
  let s = Color.to_string(color);
  Node.create_svg(
    "filter",
    [Attr.id("raised-drop-shadow-" ++ s)],
    [
      Node.create_svg(
        "feDropShadow",
        [
          Attr.classes(["tile-drop-shadow"]),
          Attr.create("dx", raised_shadow_dx),
          Attr.create("dy", raised_shadow_dy),
          Attr.create("stdDeviation", "0"),
        ],
        [],
      ),
    ],
  );
};

let shadow_filter = (~color: Color.t) => {
  let s = Color.to_string(color);
  Node.create_svg(
    "filter",
    [Attr.id("drop-shadow-" ++ s)],
    [
      Node.create_svg(
        "feDropShadow",
        [
          Attr.classes(["tile-drop-shadow"]),
          Attr.create("dx", shadow_dx),
          Attr.create("dy", shadow_dy),
          Attr.create("stdDeviation", "0"),
        ],
        [],
      ),
    ],
  );
};

let filters =
  NodeUtil.svg(
    Attr.[id("filters")],
    [
      raised_shadow_filter(~color=Exp),
      shadow_filter(~color=Exp),
      raised_shadow_filter(~color=Pat),
      shadow_filter(~color=Pat),
      raised_shadow_filter(~color=Typ),
      shadow_filter(~color=Typ),
      raised_shadow_filter(~color=Any),
      shadow_filter(~color=Any),
      raised_shadow_filter(~color=Selected),
      shadow_filter(~color=Selected),
    ],
  );

module Style = {
  type t =
    | Root(Measured.point, Measured.point)
    | Selected(int, int);

  let to_string =
    fun
    | Root(_) => "Root"
    | Selected(_) => "Selected";

  let to_selem_style =
    fun
    | Root(_) => SelemStyle.Root
    | Selected(_) => Selected;
};

module Profile = {
  type t = {
    shards: Measured.Shards.t,
    mold: Mold.t,
    style: Style.t,
  };
};

let contour_path_clss = (profile: SelemDecProfile.t) => {
  let clss = {
    let c_cls = Color.to_string(profile.color);
    let highlighted =
      SelemStyle.highlighted(profile.style) ? ["highlighted"] : [];
    let selected = SelemStyle.selected(profile.style) ? ["selected"] : [];
    List.concat([["tile-path", c_cls], highlighted, ["raised"], selected]);
  };
  clss;
};

let closed_child_path = ({origin, length}: Measured.measurement_lin) =>
  List.concat(
    SvgUtil.Path.[
      [M({x: Float.of_int(origin) +. 0.5, y: child_border_thickness})],
      Diag.tr_bl(~with_child_border=true, ~hemi=`North, ()),
      Diag.tl_br(~with_child_border=true, ~hemi=`South, ()),
      [H_({dx: Float.of_int(length - 1)})],
      Diag.bl_tr(~with_child_border=true, ~hemi=`South, ()),
      Diag.br_tl(~with_child_border=true, ~hemi=`North, ()),
      [Z],
    ],
  );

let open_child_path = ({origin, length}: Measured.measurement_lin) =>
  List.concat(
    SvgUtil.Path.[
      [H({x: Float.of_int(origin) +. tip_width})],
      Diag.tr_bl(~hemi=`North, ()),
      Diag.tl_br(~with_child_border=true, ~hemi=`South, ()),
      [H_({dx: Float.of_int(length - 1)})],
      Diag.bl_tr(~with_child_border=true, ~hemi=`South, ()),
      Diag.br_tl(~hemi=`North, ()),
    ],
  );

let contour_path = (profile: SelemDecProfile.t): SvgUtil.Path.t => {
  let lin_length =
    profile.measurement.last.col - profile.measurement.origin.col;
  let start =
    SelemStyle.stretched(profile.style) ? Float.neg(stretch_dx) : 0.;
  let end_ =
    SelemStyle.stretched(profile.style)
      ? Float.of_int(lin_length) +. stretch_dx : Float.of_int(lin_length);
  let fudge_width =
    if (profile.style == Selected) {
      0.2; //narrows piece decos to fit together clean
    } else if (fst(fst(profile.shape)).shape != Core.Nib.Shape.Convex) {
      0.22; // widen pieces with concave ends to avoid text overlap
    } else {
      0.;
    };
  List.concat(
    SvgUtil.Path.[
      [
        M({
          x:
            fudge_width
            +. start
            +. Float.of_int(profile.measurement.origin.col),
          y: 1. +. Float.of_int(profile.measurement.origin.row),
        }),
        ...Diag.left_tip_path(fst(profile.shape)),
      ],
      [
        H({
          x:
            -. fudge_width
            +. end_
            +. Float.of_int(profile.measurement.origin.col),
        }),
        ...Diag.right_tip_path(snd(profile.shape)),
      ],
      [Z],
    ],
  );
};

let shards = (profile: Profile.t) => {
  let selem_profile = (index, measurement) =>
    SelemDecProfile.{
      measurement,
      color: Color.of_sort(profile.mold.out),
      shape: piece_shape_of_nibs(Mold.nibs(~index, profile.mold)),
      style: Style.to_selem_style(profile.style),
    };
  profile.shards
  |> List.map(((i, m)) => {
       let profile = selem_profile(i, m);
       SvgUtil.Path.view(
         ~attrs=Attr.[classes(contour_path_clss(profile))],
         contour_path(profile),
       );
     });
};

let chunky = (~rows: Measured.Rows.t, (i, j), profile: Profile.t) => {
  let m_first = List.assoc(i, profile.shards);
  let m_last = List.assoc(j, profile.shards);
  let (first, last) = (m_first.origin, m_last.last);
  let indent = Measured.Rows.find(first.row, rows).indent;
  let max_col =
    ListUtil.range(~lo=first.row, last.row + 1)
    |> List.map(r => Measured.Rows.find(r, rows).max_col)
    |> List.fold_left(max, 0);
  open SvgUtil.Path;
  let l_hook = {
    let dx =
      switch (fst(Mold.nibs(~index=i, profile.mold)).shape) {
      | Convex => -. DecUtil.short_tip_width
      | Concave(_) => DecUtil.short_tip_width
      };
    let dy = (-0.5);
    [L_({dx, dy}), L_({dx: -. dx, dy})];
  };
  let r_hook = {
    let dx =
      switch (snd(Mold.nibs(~index=j, profile.mold)).shape) {
      | Convex => DecUtil.short_tip_width
      | Concave(_) => -. DecUtil.short_tip_width
      };
    let dy = 0.5;
    [L_({dx, dy}), L_({dx: -. dx, dy})];
  };
  [
    view(
      ~attrs=
        Attr.[
          classes([
            "tile-path",
            "selected",
            "raised",
            Color.to_string(Color.of_sort(profile.mold.out)),
          ]),
          create("vector-effect", "non-scaling-stroke"),
        ],
      List.concat([
        [
          m(~x=first.col, ~y=first.row),
          h(~x=max_col),
          v(~y=last.row),
          h(~x=last.col),
        ],
        r_hook,
        [h(~x=indent), v(~y=first.row + 1), h(~x=first.col)],
        l_hook,
      ]),
    ),
  ];
};

let bi_lines =
    (~rows: Measured.Rows.t, mold: Mold.t, shards: Measured.Shards.t)
    : list(Node.t) => {
  let shard_rows = Measured.Shards.split_by_row(shards);
  let intra_lines =
    shard_rows
    |> List.map(ListUtil.neighbors)
    |> List.map(
         List.map(
           (((_, l: Measured.measurement), (_, r: Measured.measurement)))
           // TODO(d) unify fudge constants
           =>
             SvgUtil.Path.[
               //1d
               M({
                 x: Float.of_int(l.last.col) +. 0.22,
                 y: Float.of_int(l.last.row + 1),
               }),
               H_({dx: Float.of_int(r.origin.col - l.last.col) -. 0.22}),
             ]
           ),
       )
    |> List.concat;
  let inter_lines =
    ListUtil.neighbors(shard_rows)
    |> List.map(
         ((row_shards: Measured.Shards.t, row_shards': Measured.Shards.t)) => {
         assert(row_shards != []);
         assert(row_shards' != []);
         let origin = snd(List.hd(row_shards)).origin;
         let origin' = snd(List.hd(row_shards')).origin;
         let indent = Measured.Rows.find(origin.row, rows).indent;
         let v_delta = origin'.col == indent ? (-1) : 0;
         SvgUtil.Path.[
           m(~x=origin.col, ~y=origin.row + 1),
           h_(~dx=indent - origin.col),
           v_(~dy=origin'.row - origin.row + v_delta),
           h_(~dx=origin'.col - indent),
         ];
       });
  intra_lines
  @ inter_lines
  |> List.map(
       SvgUtil.Path.view(
         ~attrs=
           Attr.[
             classes([
               "child-line",
               Color.to_string(Color.of_sort(mold.out)),
             ]),
             create("vector-effect", "non-scaling-stroke"),
           ],
       ),
     );
};

let uni_lines =
    (
      ~rows: Measured.Rows.t,
      (l: Measured.point, r: Measured.point),
      mold: Mold.t,
      shards: Measured.Shards.t,
    ) => {
  open SvgUtil.Path;
  let l_line = {
    let (_, m_first) = List.hd(shards);
    if (l.col != m_first.origin.col) {
      let max_col = Measured.Rows.find(l.row, rows).max_col;
      let indent = Measured.Rows.find(m_first.origin.row, rows).indent;
      [
        l.row == m_first.origin.row
          ? [
            //1d
            m(~x=m_first.origin.col, ~y=m_first.origin.row + 1),
            h(~x=l.col),
            L_({
              dx: -. DecUtil.short_tip_width,
              dy: -. DecUtil.short_tip_height,
            }),
            L_({
              dx: DecUtil.short_tip_width,
              dy: -. DecUtil.short_tip_height,
            }),
          ]
          : (
              m_first.origin.col == indent
                ? [
                  m(~x=m_first.last.col, ~y=m_first.last.row),
                  // TODO(d) need to take max of all rows, not just top
                  h(~x=max_col),
                  v(~y=l.row),
                ]
                : [
                  m(~x=m_first.origin.col, ~y=m_first.origin.row + 1),
                  h(~x=indent),
                  v(~y=l.row + 1),
                  h(~x=max_col),
                  v(~y=l.row),
                ]
            )
            @ [
              h(~x=l.col),
              L_({
                dx: -. DecUtil.short_tip_width,
                dy: DecUtil.short_tip_height,
              }),
              L_({dx: DecUtil.short_tip_width, dy: DecUtil.short_tip_height}),
            ],
      ];
    } else {
      [];
    };
  };
  let r_line = {
    let (_, m_last) = ListUtil.last(shards);
    let hook = [
      L_({
        dx: DecUtil.short_tip_width,
        dy: Float.neg(DecUtil.short_tip_height),
      }),
      L_({dx: -. DecUtil.short_tip_width, dy: -. DecUtil.short_tip_height}),
    ];
    if (r.row == m_last.last.row && r.col != m_last.last.col) {
      [
        [
          m(~x=m_last.last.col, ~y=m_last.last.row + 1),
          h(~x=r.col),
          ...hook,
        ],
      ];
    } else if (r.row != m_last.last.row) {
      let indent =
        shards
        |> List.map(((_, m): Measured.Shards.shard) =>
             Measured.Rows.find(m.origin.row, rows).indent
           )
        |> List.fold_left(min, Measured.Rows.find(r.row, rows).indent);
      // let r_indent = Measured.Rows.find(r.row, rows).indent;
      let (_, m_flast) = {
        let shard_rows = Measured.Shards.split_by_row(shards);
        assert(shard_rows != []);
        let row = ListUtil.last(shard_rows);
        assert(row != []);
        List.hd(row);
      };
      // let flast_indent = Measured.Rows.find(m_flast.origin.row, rows).indent;
      [
        [
          m(~x=m_flast.origin.col, ~y=m_flast.last.row + 1),
          h(~x=indent),
          v(~y=r.row + 1),
          h(~x=r.col),
          ...hook,
        ],
      ];
    } else {
      [];
    };
  };
  l_line
  @ r_line
  |> List.map(
       SvgUtil.Path.view(
         ~attrs=
           Attr.[
             classes([
               "child-line",
               Color.to_string(Color.of_sort(mold.out)),
             ]),
             create("vector-effect", "non-scaling-stroke"),
           ],
       ),
     );
};

let view =
    (~font_metrics: FontMetrics.t, ~rows: Measured.Rows.t, profile: Profile.t)
    : Node.t => {
  let (shards, lines) =
    switch (profile.style) {
    | Selected(i, j) => (chunky(~rows, (i, j), profile), [])
    | Root(l, r) => (
        shards(profile),
        uni_lines(~rows, (l, r), profile.mold, profile.shards)
        @ bi_lines(~rows, profile.mold, profile.shards),
      )
    };
  // let uni_lines =
  //   switch (profile.style) {
  //   | Selected => []
  //   | Root(l, r) => uni_lines(~rows, (l, r), profile.mold, profile.shards)
  //   };
  // let bi_lines =
  //   bi_lines(~rows, profile.mold, profile.shards);
  DecUtil.container2d(
    ~font_metrics,
    ~measurement={
      origin: Measured.zero,
      last: {
        row: 100,
        col: 100,
      },
    },
    ~cls="tile",
    ~container_clss=[Style.to_string(profile.style)],
    shards @ lines,
  );
};
