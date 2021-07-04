open Virtual_dom.Vdom;

let logo = (~font_metrics) => {
  let selem = (step, color: Color.t, shape: Layout.selem_shape, s): Layout.t =>
    Layout.annot(Selem({color, shape, step}), Text(s));
  let l =
    Layout.(
      spaces(
        // HACK
        Selected,
        [
          selem(0, Exp, ((Convex, 0), (Convex, 0)), "t"),
          selem(1, Pat, ((Concave, 0), (Convex, 0)), "y"),
          selem(2, Typ, ((Concave, 0), (Concave, 0)), "l"),
          selem(3, Selected, ((Convex, 0), (Concave, 1)), "r"),
        ],
      )
    );
  Code.view_of_layout(
    ~id="logo",
    ~text_id="logo-text",
    ~font_metrics,
    DecPaths.mk(~logo_selems=[0, 1, 2, 3], ()),
    l,
  );
};

let filters =
  NodeUtil.svg(
    Attr.[id("filters")],
    [
      SelemDec.raised_shadow_filter(~color=Exp),
      SelemDec.shadow_filter(~color=Exp),
      SelemDec.raised_shadow_filter(~color=Pat),
      SelemDec.shadow_filter(~color=Pat),
      SelemDec.raised_shadow_filter(~color=Typ),
      SelemDec.shadow_filter(~color=Typ),
      SelemDec.raised_shadow_filter(~color=Selected),
      SelemDec.shadow_filter(~color=Selected),
      EmptyHoleDec.inset_shadow_filter(~color=Selected),
      EmptyHoleDec.thin_inset_shadow_filter(~color=Selected),
      EmptyHoleDec.inset_shadow_filter(~color=Exp),
      EmptyHoleDec.thin_inset_shadow_filter(~color=Exp),
      EmptyHoleDec.inset_shadow_filter(~color=Pat),
      EmptyHoleDec.thin_inset_shadow_filter(~color=Pat),
      CaretPosDec.blur_filter,
    ],
  );

let view = (~inject, model: Model.t) => {
  let Model.{font_metrics, logo_font_metrics, zipper, history} = model;
  let (subject, _) = zipper;
  let dpaths = DecPaths.of_zipper(zipper);
  let l = Layout.mk_zipper(zipper);
  Node.div(
    [
      Attr.id("page"),
      // necessary to make cell focusable
      Attr.create("tabindex", "0"),
      ...Keyboard.handlers(~inject, ~zipper),
    ],
    [
      logo(~font_metrics=logo_font_metrics),
      FontSpecimen.view("font-specimen"),
      FontSpecimen.view("logo-font-specimen"),
      filters,
      Node.div(
        [Attr.id("code-container")],
        [
          BarDec.view(~font_metrics),
          Code.view_of_layout(
            ~id="under-the-rail",
            ~text_id="under-the-rail-text",
            ~font_metrics,
            ~subject,
            ~filler=Model.filler(model),
            ~just_failed=history.just_failed,
            dpaths,
            l,
          ),
        ],
      ),
    ],
  );
};
