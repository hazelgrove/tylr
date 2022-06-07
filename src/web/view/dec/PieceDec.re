open Util;
open Core;
open Virtual_dom.Vdom;

module Style = {
  type t =
    | Root(Measured.point, Measured.point)
    | Selected;

  let to_string =
    fun
    | Root(_) => "Root"
    | Selected => "Selected";

  let to_selem_style =
    fun
    | Root(_) => SelemStyle.Root
    | Selected => Selected;
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
         let origin = snd(List.hd(row_shards)).origin;
         let origin' = snd(List.hd(row_shards')).origin;
         let indent = Measured.Rows.find(origin.row, rows).indent;
         let v_delta = origin'.col == indent ? (-1) : 0;
         SvgUtil.Path.[
           M({
             x: Float.of_int(origin.col),
             y: Float.of_int(origin.row + 1),
           }),
           H_({dx: Float.of_int(indent - origin.col)}),
           V_({dy: Float.of_int(origin'.row - origin.row + v_delta)}),
           H_({dx: Float.of_int(origin'.col - indent)}),
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
      ~rows as _: Measured.Rows.t,
      (l: Measured.point, r: Measured.point),
      mold: Mold.t,
      shards: Measured.Shards.t,
    ) => {
  let (_, m_first) = List.hd(shards);
  let (_, m_last) = ListUtil.last(shards);
  open SvgUtil.Path;
  let l_line =
    if (l.col != m_first.origin.col && l.row == m_first.origin.row) {
      [
        [
          M({
            x: Float.of_int(m_first.origin.col),
            y: Float.of_int(m_first.origin.row + 1),
          }),
          H_({dx: Float.of_int(l.col - m_first.origin.col)}),
          L_({
            dx: Float.neg(DecUtil.short_tip_width),
            dy: Float.neg(DecUtil.short_tip_height),
          }),
          L_({
            dx: DecUtil.short_tip_width,
            dy: Float.neg(DecUtil.short_tip_height),
          }),
        ],
      ];
    } else {
      [];
    };
  let r_line =
    if (r.col != m_last.last.col && r.row == m_last.last.row) {
      [
        [
          M({
            x: Float.of_int(m_last.last.col),
            y: Float.of_int(m_last.last.row + 1),
          }),
          H_({dx: Float.of_int(r.col - m_last.last.col)}),
          L_({
            dx: DecUtil.short_tip_width,
            dy: Float.neg(DecUtil.short_tip_height),
          }),
          L_({
            dx: Float.neg(DecUtil.short_tip_width),
            dy: Float.neg(DecUtil.short_tip_height),
          }),
        ],
      ];
    } else {
      [];
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
  let shards = shards(~font_metrics, profile);
  let uni_lines =
    switch (profile.style) {
    | Selected => []
    | Root(l, r) => uni_lines(~rows, (l, r), profile.mold, profile.shards)
    };
  let bi_lines = bi_lines(~rows, profile.mold, profile.shards);
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
    shards @ uni_lines @ bi_lines,
  );
};
