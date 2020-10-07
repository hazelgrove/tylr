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
