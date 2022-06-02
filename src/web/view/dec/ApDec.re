open Virtual_dom.Vdom;
open DecUtil;

module Profile = {
  type t = Layout.measurement';
};

let h = 0.15;
let w = 0.6;

let view = (~font_metrics: FontMetrics.t, measurement: Profile.t) =>
  container(
    ~font_metrics,
    ~measurement,
    ~cls="ap",
    SvgUtil.Path.[
      view(
        ~attrs=[Attr.classes(["ap-path"])],
        [
          M({x: 0.5 -. w /. 2., y: 0.8 -. h}),
          V_({dy: h}),
          H_({dx: w}),
          V_({dy: -. h}),
        ],
      ),
    ],
  );
