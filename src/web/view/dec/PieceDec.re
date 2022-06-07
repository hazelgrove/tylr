open Core;
open Virtual_dom;

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

let view = (~font_metrics: FontMetrics.t, profile: Profile.t): Vdom.Node.t => {
  let shards = shards(~font_metrics, profile);
  let uni_lines = [];
  // switch (profile.style) {
  // | Selected => []
  // | Root(l, r) => failwith("todo")
  // };
  let bi_lines = [];
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
