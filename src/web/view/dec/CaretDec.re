// open Stds;
  // open Svgs;
  // let caret_width_straight = 0.1;
  // let caret_width_bent = 0.1;
  // let caret_bend = DecUtil.tip_width;
  // module Profile = {
  //   type t = {
  //     side: Dir.t,
  //     origin: Core.Measured.point,
  //     shape: option(Dir.t),
  //   };
  // };
  // let caret_path = (shape: option(Dir.t)) => {
  //   let caret_bend_param =
  //     switch (shape) {
  //     | Some(R) => -. caret_bend
  //     | Some(L) => caret_bend
  //     | None => 0.0
  //     };
  //   let caret_width_param =
  //     switch (shape) {
  //     | Some(R) => -. caret_width_bent
  //     | Some(L) => caret_width_bent
  //     | None => caret_width_straight
  //     };
  //   Path.[
  //     m(~x=0, ~y=0),
  //     H({x: caret_width_param}),
  //     L_({dx: -. caret_bend_param, dy: 0.5}),
  //     L_({dx: +. caret_bend_param, dy: 0.5}),
  //     H({x: -. caret_width_param}),
  //     L_({dx: -. caret_bend_param, dy: (-0.5)}),
  //     L_({dx: +. caret_bend_param, dy: (-0.5)}),
  //   ];
  // };
  // let view =
  //     (
  //       ~font: Font.t,
  //       ~profile as {shape, side, origin}: Profile.t,
  //     ) => {
  //   let l_adj = DecUtil.caret_adjust(side, shape);
  //   DecUtil.code_svg(
  //     ~font,
  //     ~origin,
  //     ~id="caret",
  //     ~path_cls=["caret-path"],
  //     ~height_fudge=DecUtil.shadow_adj *. font.row_height,
  //     ~left_fudge=l_adj *. font.col_width,
  //     caret_path(shape),
  //   );
  // };
