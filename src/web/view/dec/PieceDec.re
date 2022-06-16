open Util;
open Core;
open Virtual_dom.Vdom;

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

let shards = (~font_metrics, profile: Profile.t) => {
  let selem_profile = (index, measurement) =>
    SelemDec.Profile.{
      measurement,
      color: Color.of_sort(profile.mold.out),
      shape: SelemDec.piece_shape_of_nibs(Mold.nibs(~index, profile.mold)),
      style: Style.to_selem_style(profile.style),
      open_children: [],
      closed_children: [],
    };
  profile.shards
  |> List.map(((i, m)) => {
       let profile = selem_profile(i, m);
       SvgUtil.Path.view(
         ~attrs=SelemDec.contour_path_attrs(profile),
         SelemDec.contour_path(~font_metrics, profile),
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
      let r_indent = Measured.Rows.find(r.row, rows).indent;
      let (_, m_flast) = {
        let shard_rows = Measured.Shards.split_by_row(shards);
        assert(shard_rows != []);
        let row = ListUtil.last(shard_rows);
        assert(row != []);
        List.hd(row);
      };
      [
        [
          m(~x=m_flast.origin.col, ~y=m_flast.last.row + 1),
          h(~x=min(m_flast.origin.col, r_indent)),
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
        shards(~font_metrics, profile),
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
