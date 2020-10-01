type tile_shape =
  | Num(int)
  | Var(Var.t)
  | Paren
  | Lam
  | Ap
  | Ann
  | Plus
  | Arrow;

type t =
  | Mark
  | Move(Direction.t)
  | Delete
  | Construct(tile_shape);

module EditMode = {
  type t('htm, 'ztile) =
    | Normal(ZPath.t)
    | Selecting(ZPath.t, ZPath.t)
    | Restructuring(ZPath.t, ZPath.t, ZPath.t);

  let cons = two_step =>
    fun
    | Normal(focus) => Normal(ZPath.cons(two_step, focus))
    | Selecting(anchor, focus) =>
      Selecting(ZPath.cons(two_step, anchor), ZPath.cons(two_step, focus))
    | Restructuring(((l, r), target)) =>
      Restructuring(
        ZPath.cons(two_step, l),
        ZPath.cons(two_step, r),
        ZPath.cons(two_step, target),
      );

  let mark =
    fun
    | Normal(focus) => Selecting(focus, focus)
    | Selecting(anchor, focus) => Restructuring(anchor, focus, focus)
    | Restructuring(_, _, focus) => Normal(focus);

  let get_focus =
    fun
    | Normal(focus)
    | Selecting(_, focus)
    | Restructuring(_, _, focus) => focus;

  let put_focus = focus =>
    fun
    | Normal(_) => Normal(focus)
    | Selecting(anchor, _) => Selecting(anchor, focus)
    | Restructuring(l, r, _) => Restructuring(l, r, focus);

  let update_anchors = (f: ZPath.t => ZPath.t) =>
    fun
    | Normal(_) as mode => mode
    | Selecting(anchor, focus) => Selecting(f(anchor), focus)
    | Restructuring(l, r, focus) => Restructuring(f(l), f(r), focus);
};

module EditState = {
  type zipper = [
    | `Exp(ZExp.zipper)
    | `Pat(ZPat.zipper)
    | `Typ(ZTyp.zipper)
  ];
  type t = (EditMode.t, zipper);
};

open OptUtil.Syntax;

let rec perform = (a: t, edit_state: EditState.t): option(EditState.t) =>
  switch (a) {
  | Move(d) =>
    let+ (focus, zipped: option((ZPath.two_step, zipper))) = {
      let move =
        switch (zipper) {
        | `Exp(zipper) => ZPath.Exp.move(d, zipper)
        | `Pat(zipper) => ZPath.Pat.move(d, zipper)
        | `Typ(zipper) => ZPath.Typ.move(d, zipper)
        };
      move(EditMode.get_focus(mode));
    };
    let mode = EditMode.put_focus(focus, mode);
    switch (zipped) {
    | None => (mode, zipper)
    | Some((two_step, zipper)) => (
        EditMode.update_anchors(ZPath.cons(two_step), mode),
        zipper,
      )
    };

  | (Delete, Exp(Normal((steps, k), e, zrest))) =>
    switch (steps) {
    | [] =>
      let (prefix, suffix) = ListUtil.split_n(k, e);
      switch (ListUtil.split_last_opt(prefix)) {
      | None =>
        // TODO enter restructuring mode if possible
        None
      | Some((prefix, tile)) =>
        let open_children_tiles =
          List.flatten(HExp.Tile.get_open_children(tile));
        let (e, k) = {
          let (prefix, suffix) =
            HExp.fix_empty_holes(prefix @ open_children_tiles, suffix);
          (prefix @ suffix, List.length(prefix));
        };
        let+ {ctx, mode} = ZInfo.Exp.mk(ztile);
        switch (mode) {
        | Syn({fn_pos, fix}) =>
          let (e, ty) = Statics.Exp.syn_fix_holes(ctx, e);
          let ze = fix(ty);
          // TODO resolve zexp ambiguity
          Exp(Normal(([], k), e, Option.get(ze.z)));
        | Ana({expected, fixed}) =>
          let e = Statics.Exp.syn_fix_holes(ctx, e, expected);
          Exp(Normal(([], k), e, Option.get(fixed.z)));
        };
      };
    | [two_step, ...steps] =>
      let* unzipped = ZPath.Exp.unzip(~init=ztile, two_step, e);
      switch (unzipped) {
      | Pat(p, ztile) => perform(a, Pat(Normal((steps, k), p, ztile)))
      | Exp(e, ztile) => perform(a, Exp(Normal((steps, k), e, ztile)))
      };
    }
  };
