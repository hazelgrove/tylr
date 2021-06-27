open DecConstants;

// top right to bottom left
let tr_bl =
    (
      ~scale as s=1.,
      ~hemi: [ | `North | `South],
      ~with_child_border=false,
      ~stretch_x=0.,
      ~stretch_y=0.,
      (),
    ) =>
  SvgUtil.Path.(
    {
      let (diag, junction) =
        with_child_border
          ? (
            L_({dx: Float.neg(short_tip_width), dy: short_tip_height}),
            H_({dx: Float.neg(0.5 -. short_tip_width)}),
          )
          : (
            L_({dx: Float.neg(tip_width), dy: 0.5 +. stretch_y}),
            H_({dx: Float.neg(stretch_x)}),
          );
      let path =
        switch (hemi) {
        | `North => [junction, diag]
        | `South => [diag, junction]
        };
      scale(s, path);
    }
  );
// bottom left to top right
let bl_tr =
    (
      ~hemi: [ | `North | `South],
      ~with_child_border=false,
      ~stretch_x=0.,
      ~stretch_y=0.,
      (),
    ) =>
  SvgUtil.Path.reverse(
    tr_bl(~hemi, ~with_child_border, ~stretch_x, ~stretch_y, ()),
  );

// top left to bottom right
let tl_br =
    (
      ~hemi: [ | `North | `South],
      ~with_child_border=false,
      ~stretch_x=0.,
      ~stretch_y=0.,
      (),
    ) =>
  SvgUtil.Path.(
    {
      let (diag, junction) =
        with_child_border
          ? (
            L_({dx: short_tip_width, dy: short_tip_height}),
            H_({dx: 0.5 -. short_tip_width}),
          )
          : (L_({dx: tip_width, dy: 0.5 +. stretch_y}), H_({dx: stretch_x}));
      switch (hemi) {
      | `North => [junction, diag]
      | `South => [diag, junction]
      };
    }
  );
// bottom right to top left
let br_tl =
    (
      ~hemi: [ | `North | `South],
      ~with_child_border=false,
      ~stretch_x=0.,
      ~stretch_y=0.,
      (),
    ) =>
  SvgUtil.Path.reverse(
    tl_br(~hemi, ~with_child_border, ~stretch_x, ~stretch_y, ()),
  );
