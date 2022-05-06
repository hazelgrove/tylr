open Virtual_dom.Vdom;
open DecUtil;
// open Core;

module Profile = {
  type t = {
    measurement: Layout.measurement,
    color: Color.t,
    // TODO refactor types around new Nib datatype
    // tip: Nib.shape,
  };
};

let path = (tip, offset, s: float) => {
  let x_dilate = 1.5;
  List.concat(
    SvgUtil.Path.[
      [
        M({x: offset +. 0.5, y: 0.5 -. s /. 2.}),
        H_({dx: x_dilate *. s /. 2.}),
      ],
      Diag.right_tip_path(~scale_x=s *. x_dilate, ~scale_y=s, (tip, 0)),
      [H_({dx: -. s *. x_dilate})],
      Diag.left_tip_path(~scale_x=s *. x_dilate, ~scale_y=s, (tip, 0)),
      [Z],
    ],
  );
};

let view = (~font_metrics, {measurement, color}: Profile.t): Node.t => {
  let c_cls = Color.to_string(color);
  //let _ = failwith("fix EmptyHoleDec");
  let tip: Core.Nib.t = {shape: Convex, sort: Exp};
  container(
    ~font_metrics,
    ~measurement,
    ~cls="empty-hole",
    SvgUtil.Path.[
      view(
        ~attrs=[Attr.classes(["empty-hole-path", c_cls])],
        //[],
        path(tip, 0., 0.28),
      ),
    ],
  );
};
