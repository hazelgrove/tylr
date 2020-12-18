module Mode = {
  [@deriving sexp]
  type t =
    | Normal(ZPath.t)
    | Selecting(ZPath.anchored_selection)
    | Restructuring(ZPath.ordered_selection, ZPath.t);

  let cons = two_step =>
    fun
    | Normal(focus) => Normal(ZPath.cons(two_step, focus))
    | Selecting(selection) =>
      Selecting(ZPath.cons_anchored_selection(two_step, selection))
    | Restructuring(selection, focus) =>
      Restructuring(
        ZPath.cons_ordered_selection(two_step, selection),
        ZPath.cons(two_step, focus),
      );

  let get_focus =
    fun
    | Normal(focus)
    | Selecting({focus, _})
    | Restructuring(_, focus) => focus;

  let put_focus = focus =>
    fun
    | Normal(_) => Normal(focus)
    | Selecting({origin, anchor, _}) => Selecting({origin, anchor, focus})
    | Restructuring(selection, _) => Restructuring(selection, focus);

  let update_anchors = (f: ZPath.t => ZPath.t) =>
    fun
    | Normal(_) as mode => mode
    | Selecting({origin, anchor, focus}) =>
      Selecting({origin: f(origin), anchor: f(anchor), focus})
    | Restructuring((l, r), focus) => Restructuring((f(l), f(r)), focus);
};

module Zipper = {
  [@deriving sexp]
  type t = [ | `Exp(ZExp.zipper) | `Pat(ZPat.zipper) | `Typ(ZTyp.zipper)];
  type did_it_zip = option((ZPath.two_step, t));

  let zip = (zipper: t): option((ZPath.two_step, t)) =>
    switch (zipper) {
    | `Typ(_, None)
    | `Pat(_, None)
    | `Exp(_, None) => None
    | `Typ(ty, Some(ztile)) =>
      let (two_step, zip_result) = ZPath.Typ.zip_ztile(ty, ztile);
      Some((two_step, (zip_result :> t)));
    | `Pat(p, Some(ztile)) =>
      let (two_step, zip_result) = ZPath.Pat.zip_ztile(p, ztile);
      Some((two_step, (zip_result :> t)));
    | `Exp(e, Some(ztile)) =>
      let (two_step, zip_result) = ZPath.Exp.zip_ztile(e, ztile);
      Some((two_step, (zip_result :> t)));
    };

  let unzip = (two_step, zipper: t) =>
    switch (zipper) {
    | `Typ(zipper) => (ZPath.Typ.unzip(two_step, zipper) :> t)
    | `Pat(zipper) => (ZPath.Pat.unzip(two_step, zipper) :> t)
    | `Exp(zipper) => (ZPath.Exp.unzip(two_step, zipper) :> t)
    };

  let sort_at = (path, zipper: t) =>
    switch (zipper) {
    | `Typ(ty, _) => ZPath.Typ.sort_at(path, ty)
    | `Pat(p, _) => ZPath.Pat.sort_at(path, p)
    | `Exp(e, _) => ZPath.Exp.sort_at(path, e)
    };

  let length_at = (path, zipper: t) =>
    switch (zipper) {
    | `Typ(ty, _) => ZPath.Typ.length_at(path, ty)
    | `Pat(p, _) => ZPath.Pat.length_at(path, p)
    | `Exp(e, _) => ZPath.Exp.length_at(path, e)
    };

  open Util.OptUtil.Syntax;

  let move = (d: Direction.t, path: ZPath.t, zipper: t) =>
    switch (zipper) {
    | `Exp(zipper) => (
        ZPath.Exp.move_zipper(d, path, zipper) :>
          option((ZPath.t, did_it_zip))
      )
    | `Pat(zipper) => (
        ZPath.Pat.move_zipper(d, path, zipper) :>
          option((ZPath.t, did_it_zip))
      )
    | `Typ(zipper) => (
        ZPath.Typ.move_zipper(d, path, zipper) :>
          option((ZPath.t, did_it_zip))
      )
    };

  let delete_selection = (selection, zipper: t) =>
    switch (zipper) {
    | `Typ(ty, unzipped) =>
      let+ (path, ty) = ZPath.Typ.delete_selection(selection, ty);
      (path, `Typ((ty, unzipped)));
    | `Pat(p, unzipped) =>
      let+ (path, p) = ZPath.Pat.delete_selection(selection, p);
      (path, `Pat((p, unzipped)));
    | `Exp(e, unzipped) =>
      let+ (path, e) = ZPath.Exp.delete_selection(selection, e);
      (path, `Exp((e, unzipped)));
    };

  let restructure = (selection, target, zipper: t) =>
    switch (zipper) {
    | `Typ(ty, unzipped) =>
      let+ (path, ty) = ZPath.Typ.restructure(selection, target, ty);
      (path, `Typ((ty, unzipped)));
    | `Pat(p, unzipped) =>
      let+ (path, p) = ZPath.Pat.restructure(selection, target, p);
      (path, `Pat((p, unzipped)));
    | `Exp(e, unzipped) =>
      let+ (path, e) = ZPath.Exp.restructure(selection, target, e);
      (path, `Exp((e, unzipped)));
    };
};

[@deriving sexp]
type t = (Mode.t, Zipper.t);

let rec zip_up = ((mode, zipper) as edit_state: t): t =>
  switch (zipper) {
  | `Typ(_, None)
  | `Pat(_, None)
  | `Exp(_, None) => edit_state
  | `Typ(ty, Some(ztile)) =>
    let (two_step, zipped) = ZPath.Typ.zip_ztile(ty, ztile);
    let mode = Mode.cons(two_step, mode);
    zip_up((mode, (zipped :> Zipper.t)));
  | `Pat(p, Some(ztile)) =>
    let (two_step, zipped) = ZPath.Pat.zip_ztile(p, ztile);
    let mode = Mode.cons(two_step, mode);
    zip_up((mode, (zipped :> Zipper.t)));
  | `Exp(e, Some(ztile)) =>
    let (two_step, zipped) = ZPath.Exp.zip_ztile(e, ztile);
    let mode = Mode.cons(two_step, mode);
    zip_up((mode, (zipped :> Zipper.t)));
  };

let rec unzip_down = ((mode, zipper) as edit_state: t): t =>
  switch (mode) {
  | Normal(([two_step, ...steps], j)) =>
    unzip_down((Normal((steps, j)), Zipper.unzip(two_step, zipper)))
  | Selecting({
      origin: ([two_step_o, ...steps_o], j_o),
      anchor: ([two_step_a, ...steps_a], j_a),
      focus: ([two_step_f, ...steps_f], j_f),
    })
      when two_step_o == two_step_a && two_step_a == two_step_f =>
    unzip_down((
      Selecting({
        origin: (steps_o, j_o),
        anchor: (steps_a, j_a),
        focus: (steps_f, j_f),
      }),
      Zipper.unzip(two_step_o, zipper),
    ))
  | Restructuring(
      (([two_step_l, ...steps_l], j_l), ([two_step_r, ...steps_r], j_r)),
      ([two_step_t, ...steps_t], j_t),
    )
      when two_step_l == two_step_r && two_step_r == two_step_t =>
    unzip_down((
      Restructuring(((steps_l, j_l), (steps_r, j_r)), (steps_t, j_t)),
      Zipper.unzip(two_step_l, zipper),
    ))
  | _ => edit_state
  };
