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
  | Delete(Direction.t)
  | Construct(tile_shape);

module EditMode = {
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

  let mark =
    fun
    | Normal(focus) => Selecting((focus, focus))
    | Selecting((l, _) as selection) => Restructuring(selection, l)
    | Restructuring(_, focus) => Normal(focus);

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

module EditState = {
  type zipper = [
    | `Exp(ZExp.zipper)
    | `Pat(ZPat.zipper)
    | `Typ(ZTyp.zipper)
  ];
  type did_it_zip = option((ZPath.two_step, zipper));

  type t = (EditMode.t, zipper);
};

open OptUtil.Syntax;

let rec perform = (a: t, edit_state: EditState.t): option(EditState.t) =>
  switch (a, edit_state) {
  | (Move(d), (mode, zipper)) =>
    let+ (focus, did_it_zip) = {
      let move = path =>
        switch (zipper) {
        | `Exp(zipper) => (
            ZPath.Exp.move(d, path, zipper) :>
              option((ZPath.t, EditState.did_it_zip))
          )
        | `Pat(zipper) => (
            ZPath.Pat.move(d, path, zipper) :>
              option((ZPath.t, EditState.did_it_zip))
          )
        | `Typ(zipper) => (
            ZPath.Typ.move(d, path, zipper) :>
              option((ZPath.t, EditState.did_it_zip))
          )
        };
      move(EditMode.get_focus(mode));
    };
    let mode = EditMode.put_focus(focus, mode);
    switch (did_it_zip) {
    | None => (mode, zipper)
    | Some((two_step, zipped)) => (
        EditMode.update_anchors(ZPath.cons(two_step), mode),
        zipped,
      )
    };

  | (Delete(d), (Normal(focus), zipper)) =>
    switch (zipper) {
    | `Typ(z) =>
      let+ (selection, did_it_zip) = ZPath.Typ.select(d, focus, z);
      let zipper =
        switch (did_it_zip) {
        | None => zipper
        | Some((_, zipped)) => (zipped :> EditState.zipper)
        };
      (EditMode.Selecting(selection), zipper);
    | `Pat(z) =>
      let+ (selection, did_it_zip) = ZPath.Pat.select(d, focus, z);
      let zipper =
        switch (did_it_zip) {
        | None => zipper
        | Some((_, zipped)) => (zipped :> EditState.zipper)
        };
      (EditMode.Selecting(selection), zipper);
    | `Exp(z) =>
      let+ (selection, did_it_zip) = ZPath.Exp.select(d, focus, z);
      let zipper =
        switch (did_it_zip) {
        | None => zipper
        | Some((_, zipped)) => (zipped :> EditState.zipper)
        };
      (EditMode.Selecting(selection), zipper);
    }

  | (
      Delete(_) | Construct(_),
      (
        Selecting((
          ([two_step_l, ...steps_l], j_l),
          ([two_step_r, ...steps_r], j_r),
        )),
        zipper,
      ),
    )
      when two_step_l == two_step_r =>
    let mode = EditMode.Selecting(((steps_l, j_l), (steps_r, j_r)));
    let zipper =
      switch (zipper) {
      | `Typ(zipper) => (
          ZPath.Typ.unzip(two_step_l, zipper) :> EditState.zipper
        )
      | `Pat(zipper) => (
          ZPath.Pat.unzip(two_step_l, zipper) :> EditState.zipper
        )
      | `Exp(zipper) => (
          ZPath.Exp.unzip(two_step_l, zipper) :> EditState.zipper
        )
      };
    perform(a, (mode, zipper));

  | (Delete(d), (Selecting((l, r)), zipper)) =>
    let c = ZPath.compare(l, r);
    if (c == 0) {
      Some((Normal(r), zipper));
    } else if (c > 0) {
      perform(a, Selecting((r, l), zipper));
    } else {
      switch (zipper) {
      | `Typ(zipper) =>
        let+ (selection_l, did_it_zip_l) = ZPath.Typ.select(Right, l, zipper)
        and+ (selection_r, did_it_zip_r) = ZPath.Typ.select(Left, r, zipper);
        let min_l = ZPath.min(fst(selection_l), fst(selection_r));
        let max_r = ZPath.max(snd(selection_l), snd(selection_r));
        switch (did_it_zip_l, did_it_zip_r) {
        | (None, None) => (
            Restructuring((min_l, max_r), min_l),
            `Typ(zipper),
          )
        };
      };
    };

  | (Construct(_), (Restructuring(_), _)) => None

  | (Delete(_), (Restructuring(selection, _), zipper)) =>
    switch (zipper) {
    | `Typ(ty, unzipped) =>
      let+ (path, ty) = ZPath.Typ.remove_selection(selection, ty);
      (Normal(path), `Typ((ty, unzipped)));
    | `Pat(p, unzipped) =>
      let+ (path, p) = ZPath.Pat.remove_selection(selection, p);
      (Normal(path), `Pat((p, unzipped)));
    | `Exp(e, unzipped) =>
      let+ (path, e) = ZPath.Exp.remove_selection(selection, p);
      (Normal(path), `Exp((e, unzipped)));
    }

  | (Mark, (Restructuring(selection, target), zipper)) =>
    switch (zipper) {
    | `Typ(ty, unzipped) =>
      let+ (path, ty) = ZPath.Typ.restructure(selection, target, ty);
      (Normal(path), `Typ((ty, unzipped)));
    | `Pat(p, unzipped) =>
      let+ (path, p) = ZPath.Pat.restructure(selection, target, p);
      (Normal(path), `Pat((p, unzipped)));
    | `Exp(e, unzipped) =>
      let+ (path, e) = ZPath.Exp.restructure(selection, target, e);
      (Normal(path), `Exp((e, unzipped)));
    }
  };
