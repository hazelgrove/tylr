module Mode = {
  type t =
    | Normal(ZPath.t)
    | Selecting(ZPath.selection)
    | Restructuring(ZPath.selection, ZPath.t);

  let cons = two_step =>
    fun
    | Normal(focus) => Normal(ZPath.cons(two_step, focus))
    | Selecting(selection) =>
      Selecting(ZPath.cons_selection(two_step, selection))
    | Restructuring(selection, focus) =>
      Restructuring(
        ZPath.cons_selection(two_step, selection),
        ZPath.cons(two_step, focus),
      );

  let get_focus =
    fun
    | Normal(focus)
    | Selecting((_, focus))
    | Restructuring(_, focus) => focus;

  let put_focus = focus =>
    fun
    | Normal(_) => Normal(focus)
    | Selecting((anchor, _)) => Selecting((anchor, focus))
    | Restructuring(selection, _) => Restructuring(selection, focus);

  let update_anchors = (f: ZPath.t => ZPath.t) =>
    fun
    | Normal(_) as mode => mode
    | Selecting((anchor, focus)) => Selecting((f(anchor), focus))
    | Restructuring((l, r), focus) => Restructuring((f(l), f(r)), focus);
};

module Zipper = {
  type t = [ | `Exp(ZExp.zipper) | `Pat(ZPat.zipper) | `Typ(ZTyp.zipper)];
  type did_it_zip = option((ZPath.two_step, t));
};

type t = (Mode.t, Zipper.t);

let zip_up = ((mode, zipper) as edit_state: t): t =>
  switch (zipper) {
  | `Typ(_, None)
  | `Pat(_, None)
  | `Exp(_, None) => edit_state
  | `Typ(ty, Some(ztile)) =>
    let (two_step, zipped) = ZPath.Typ.zip_ztile(ty, ztile);
    let mode = Mode.cons(two_step, mode);
    (mode, (zipped :> Zipper.t));
  | `Pat(p, Some(ztile)) =>
    let (two_step, zipped) = ZPath.Pat.zip_ztile(p, ztile);
    let mode = Mode.cons(two_step, mode);
    (mode, (zipped :> Zipper.t));
  | `Exp(e, Some(ztile)) =>
    let (two_step, zipped) = ZPath.Exp.zip_ztile(e, ztile);
    let mode = Mode.cons(two_step, mode);
    (mode, (zipped :> Zipper.t));
  };
