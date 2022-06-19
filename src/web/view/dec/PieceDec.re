open Util;
open Core;
open Virtual_dom.Vdom;
open Node;
open DecUtil;
open SvgUtil;

[@deriving show]
type piece_shape = (Diag.tip_shape, Diag.tip_shape);

let raised_shadow_filter = (sort: Sort.t) => {
  let s = Sort.to_string(sort);
  create_svg(
    "filter",
    [Attr.id("raised-drop-shadow-" ++ s)],
    [
      create_svg(
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

let shadow_filter = (sort: Sort.t) => {
  let s = Sort.to_string(sort);
  create_svg(
    "filter",
    [Attr.id("drop-shadow-" ++ s)],
    [
      create_svg(
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
    List.map(raised_shadow_filter, Sort.all)
    @ List.map(shadow_filter, Sort.all),
  );

module Style = {
  type t =
    | Root(Measured.point, Measured.point)
    | Selected(int, int);
};

module Profile = {
  type t = {
    shards: Measured.Shards.t,
    mold: Mold.t,
    style: Style.t,
  };
};

let simple_shard_path = ((l, r): Nibs.shapes, length: int) => {
  let (l_run, l_adj) =
    switch (l) {
    | Convex => (DecUtil.tip_width, DecUtil.convex_adj)
    | Concave(_) => (-. DecUtil.tip_width, DecUtil.concave_adj)
    };
  let (r_run, r_adj) =
    switch (r) {
    | Convex => (-. DecUtil.tip_width, DecUtil.convex_adj)
    | Concave(_) => (DecUtil.tip_width, DecUtil.concave_adj)
    };
  let length = float_of_int(length) +. l_adj +. r_adj;
  Path.[
    M({x: -. l_adj, y: 0.}),
    H_({dx: length}),
    L_({dx: -. r_run, dy: 0.5}),
    L_({dx: +. r_run, dy: 0.5}),
    H_({dx: -. length}),
    L_({dx: -. l_run, dy: (-0.5)}),
    L_({dx: +. l_run, dy: (-0.5)}),
  ];
};

let simple_shard =
    (
      ~profile as {mold, _}: Profile.t,
      ~font_metrics: FontMetrics.t,
      (index, {origin, last}: Measured.measurement),
    )
    : t => {
  let nib_shapes = Mold.nib_shapes(index, mold);
  let path = simple_shard_path(nib_shapes, last.col - origin.col);
  let clss = ["tile-path", "raised", "indicated", Sort.to_string(mold.out)];
  DecUtil.code_svg(~font_metrics, ~origin, ~path_cls=clss, path);
};

let chunky_shard_path =
    (
      origin: Measured.point,
      last: Measured.point,
      (l, r): Nibs.shapes,
      indent: int,
      max_col: int,
    )
    : list(Path.cmd) => {
  //TODO(andrew): update with shape adjustments
  let l_hook = {
    let dx =
      switch (l) {
      | Convex => -. DecUtil.short_tip_width
      | Concave(_) => DecUtil.short_tip_width
      };
    let dy = (-0.5);
    Path.[L_({dx, dy}), L_({dx: -. dx, dy})];
  };
  let r_hook = {
    let dx =
      switch (r) {
      | Convex => DecUtil.short_tip_width
      | Concave(_) => -. DecUtil.short_tip_width
      };
    let dy = 0.5;
    Path.[L_({dx, dy}), L_({dx: -. dx, dy})];
  };
  List.concat(
    Path.[
      [
        m(~x=0, ~y=0),
        h(~x=max_col - origin.col + 1),
        v(~y=last.row - origin.row),
        h(~x=last.col - origin.col),
      ],
      r_hook,
      [h(~x=indent - origin.col), v(~y=1), h(~x=0)],
      l_hook,
    ],
  );
};

let chunky_shard =
    (
      ~font_metrics: FontMetrics.t,
      ~rows: Measured.Rows.t,
      (i, j),
      {shards, mold, _}: Profile.t,
    ) => {
  let (origin, last) = (
    List.assoc(i, shards).origin,
    List.assoc(j, shards).last,
  );
  let (nib_l, _) = Mold.nib_shapes(i, mold);
  let (_, nib_r) = Mold.nib_shapes(j, mold);
  let indent = Measured.Rows.find(origin.row, rows).indent;
  let max_col =
    ListUtil.range(~lo=origin.row, last.row + 1)
    |> List.map(r => Measured.Rows.find(r, rows).max_col)
    |> List.fold_left(max, 0);
  let path =
    chunky_shard_path(origin, last, (nib_l, nib_r), indent, max_col);
  let clss = ["tile-path", "selected", "raised", Sort.to_string(mold.out)];
  DecUtil.code_svg(~font_metrics, ~origin, ~path_cls=clss, path);
};

let bi_lines =
    (
      ~font_metrics: FontMetrics.t,
      ~rows: Measured.Rows.t,
      mold: Mold.t,
      shards: Measured.Shards.t,
    )
    : list(t) => {
  let shard_rows = Measured.Shards.split_by_row(shards);
  let intra_lines =
    shard_rows
    |> List.map(ListUtil.neighbors)
    |> List.map(
         List.map(
           (((_, l: Measured.measurement), (_, r: Measured.measurement))) =>
           (
             l.origin,
             SvgUtil.Path.[
               M({x: 0., y: 1. +. DecUtil.shadow_adj}),
               H({x: Float.of_int(r.last.col - l.origin.col)}),
             ],
           )
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
         (
           origin,
           SvgUtil.Path.[
             m(~x=0, ~y=0 + 1),
             h_(~dx=indent - origin.col),
             v_(~dy=origin'.row - origin.row + v_delta),
             h_(~dx=origin'.col - indent),
           ],
         );
       });
  let clss = ["child-line", Sort.to_string(mold.out)];
  intra_lines
  @ inter_lines
  |> List.map(((origin, path)) =>
       DecUtil.code_svg(~font_metrics, ~origin, ~path_cls=clss, path)
     );
};

let uni_lines =
    (
      ~font_metrics: FontMetrics.t,
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
          ? (
            m_first.origin,
            [
              M({x: 0., y: 1. +. DecUtil.shadow_adj}),
              h(~x=l.col - m_first.origin.col),
              L_({
                dx: -. DecUtil.short_tip_width,
                dy: -. DecUtil.short_tip_height,
              }),
              L_({
                dx: DecUtil.short_tip_width,
                dy: -. DecUtil.short_tip_height,
              }),
            ],
          )
          : (
            m_first.origin,
            (
              m_first.origin.col == indent
                ? [
                  m(~x=0, ~y=0),
                  // TODO(d) need to take max of all rows, not just top
                  h(~x=max_col - m_first.origin.col),
                  v(~y=l.row - m_first.origin.row),
                ]
                : [
                  M({x: 0., y: 1. +. DecUtil.shadow_adj}),
                  h(~x=indent - m_first.origin.col),
                  v(~y=l.row + 1 - m_first.origin.row),
                  h(~x=max_col - m_first.origin.col),
                  v(~y=l.row - m_first.origin.row),
                ]
            )
            @ [
              h(~x=l.col - m_first.origin.col),
              L_({
                dx: -. DecUtil.short_tip_width,
                dy: DecUtil.short_tip_height,
              }),
              L_({dx: DecUtil.short_tip_width, dy: DecUtil.short_tip_height}),
            ],
          ),
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
        (
          m_last.origin,
          // 1d?
          [
            M({
              x: float_of_int(m_last.last.col - m_last.origin.col),
              y:
                float_of_int(m_last.last.row - m_last.origin.row + 1)
                +. DecUtil.shadow_adj,
            }),
            h(~x=r.col - m_last.origin.col),
            ...hook,
          ],
        ),
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
        (
          m_flast.origin,
          [
            M({
              x: 0.,
              y:
                float_of_int(m_flast.last.row - m_flast.origin.row + 1)
                +. DecUtil.shadow_adj,
            }),
            h(~x=indent - m_flast.origin.col),
            v(~y=r.row - m_flast.origin.row + 1),
            h(~x=r.col - m_flast.origin.col),
            ...hook,
          ],
        ),
      ];
    } else {
      [];
    };
  };
  let clss = ["child-line", Sort.to_string(mold.out)];
  l_line
  @ r_line
  |> List.map(((origin, path)) =>
       DecUtil.code_svg(~font_metrics, ~origin, ~path_cls=clss, path)
     );
};

let view =
    (
      ~font_metrics: FontMetrics.t,
      ~rows: Measured.Rows.t,
      {mold, shards, _} as profile: Profile.t,
    )
    : t => {
  let svgs =
    switch (profile.style) {
    | Selected(i, j) => [
        chunky_shard(~font_metrics, ~rows, (i, j), profile),
      ]
    | Root(l, r) =>
      List.map(simple_shard(~profile, ~font_metrics), profile.shards)
      @ uni_lines(~font_metrics, ~rows, (l, r), mold, shards)
      @ bi_lines(~font_metrics, ~rows, mold, shards)
    };
  div([], svgs);
};
