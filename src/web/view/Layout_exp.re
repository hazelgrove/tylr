open Layout;

let rec mk_frame =
        (~show_err_holes: bool, frame: Frame_exp.t)
        : TypeInfo_exp.t(Layout.frame) => {
  let rec go = (frame: Frame_exp.t) =>
    switch (frame) {
    | Uni(uni) => mk_unidelimited(uni)
    | Bi(bi) => mk_bidelimited(bi)
    }
  and mk_unidelimited = (uni: Frame_exp.unidelimited) =>
    switch (uni) {
    | Pre_r(Lam(p), frame) =>
      let info = mk_frame(frame);
      let info_p = TypeInfo_exp.lam_pat(info);
      let l_p = Layout_pat.mk(info_p, p);
      let (ty_p, ctx_body) = Statics_pat.syn(info_p, p);
      let mode_body: TypeInfo_exp.mode(_) = {
        let l_lam = l_body => cat(grouts_l([mk_Lam(l_p)]), l_body);
        switch (info.mode) {
        | Syn(l_frame) =>
          Syn((ty_body, l_body) => l_frame(ty_body, l_lam(l_body)))
        | Fn_pos(l_frame) =>
          Syn((ty_body, l_body) => l_frame(ty_p, ty_body, l_lam(l_body)))
        | Ana(ty, l_frame) =>
          switch (Type.matches_arrow(ty)) {
          | None =>
            Syn(
              (_ty_body, l_body) => l_frame(Annot(ErrHole, l_lam(l_body))),
            )
          | Some((_, ty_out)) => Ana(ty_out, l_frame)
          }
        };
      };
      {ctx: ctx_body, mode: mode_body};
    | Pre_r(Let(p, def), frame) =>
      let info = mk_frame(frame);
      let info_p = TypeInfo_pat.{ctx: info.ctx, mode: Syn()};
      let l_p = Layout_pat.mk_term(info_p, p);
      let l_def = {
        let (p_ty, _) = Statics_pat.syn(info_p, p);
        let info_def = TypeInfo_exp.{ctx: info.ctx, mode: Ana(p_ty, ())};
        mk_term(info_def, def);
      };
      let ctx_body = Statics_exp.extend_ctx_let_body(info.ctx, p, def);
      let mode_body =
        info.mode
        |> TypeInfo_exp.map_mode(
             (l_frame, l_body) =>
               l_frame(cat(grouts_l([mk_Let(l_p, l_def)]), l_body)),
             {ctx: ctx_body, mode: mode_body},
           );
      {ctx: ctx_body, mode: mode_body};
    | Post_r(_, Ap(_)) => failwith("ap todo")
    | Bin_l(frame, Plus, r) =>
      let info = mk_frame(frame);
      let l_r = mk_term({ctx: info.ctx, mode: Ana(Num, ())}, r);
      let l_plus = l_l => cats([l_l, mk_Plus(), l_r]);
      let mode_l =
        switch (info.mode) {
        | Syn(l_frame) => Ana(Num, l_l => l_frame(Num, l_plus(l_l)))
        | Ana(ty, l_frame) =>
          Ana(
            Num,
            l_l =>
              err_hole(
                show_err_holes && !Type.consistent(ty, Num),
                l_frame(l_plus(l_l)),
              ),
          )
        | Fn_pos(l_frame) =>
          Ana(
            Num,
            l_l =>
              err_hole(show_err_holes, l_frame(Hole, Hole, l_plus(l_l))),
          )
        };
      {...info, mode: mode_l};
    | Bin_r(l, Plus, frame) =>
      let info = mk_frame(frame);
      let l_l = mk_term({ctx: info.ctx, mode: Ana(Num, ())}, l);
      let l_plus = l_r => cats([l_l, mk_Plus(), l_r]);
      let mode_r =
        switch (info.mode) {
        | Syn(l_frame) => Ana(Num, l_r => l_frame(Num, l_plus(l_r)))
        | Ana(ty, l_frame) =>
          Ana(
            Num,
            l_r =>
              err_hole(
                show_err_holes && !Type.consistent(ty, Num),
                l_frame(l_plus(l_r)),
              ),
          )
        | Fn_pos(l_frame) =>
          Ana(
            Num,
            l_r =>
              err_hole(show_err_holes, l_frame(Hole, Hole, l_plus(l_r))),
          )
        };
      {...info, mode: mode_r};
    | Bin_l(_, BinHole, _)
    | Bin_r(_, BinHole, _) => failwith("binhole todo")
    };
  go(frame);
};

let root_tile = root_tile(~sort=Exp);
let uni_child = uni_child(~sort=Exp);

let rec mk_term =
        (
          ~has_caret: option(CaretPosition.t)=?,
          info: TypeInfo_exp.t,
          e: Term_exp.t,
        )
        : Layout.t => {
  open Term_exp;
  let caret = Pointing(info);
  let has_caret = Option.map(pos => (caret, pos), has_caret);
  let decorate = (f_op, f_pre, f_post, f_bin) =>
    Term.get(
      op => {
        let root_tile = root_tile(~shape=Op(Term_exp.is_op_hole(op)));
        let (op, dangling_caret) = f_op(op);
        switch (dangling_caret) {
        | None => grouts([op])
        | Some(Before) => grouts_z(([], (), [root_tile(op)]))
        | Some(After) => grouts_z(([root_tile(op)], (), []))
        };
      },
      pre => {
        let root_tile = root_tile(~shape=Pre());
        let ((pre, dangling_caret), r) = f_pre(pre);
        switch (dangling_caret) {
        | None => cat(grouts_l([pre]), r)
        | Some(side) =>
          let r = uni_child(~side=Right, r);
          switch (side) {
          | Left => cats([grout(~caret, ()), root_tile(pre), r])
          | Right =>
            cat(grouts_l([root_tile(pre)]), place_caret_before(caret, r))
          };
        };
      },
      post => {
        let root_tile = root_tile(~shape=Post());
        let (l, (post, dangling_caret)) = f_post(post);
        switch (dangling_caret) {
        | None => cat(l, grouts_r([post]))
        | Some(side) =>
          let l = uni_child(~side=Left, l);
          switch (side) {
          | Before =>
            cat(place_caret_after(caret, l), grouts_r([root_tile(post)]))
          | After => cats([l, root_tile(post), grout(~caret, ())])
          };
        };
      },
      bin => {
        let root_tile = root_tile(~shape=Bin(Term_exp.is_bin_hole(bin)));
        let (l, (bin, dangling_caret), r) = f_bin(bin);
        switch (dangling_caret) {
        | None => cats([l, bin, r])
        | Some(side) =>
          let l = uni_child(~side=Left, l);
          let r = uni_child(~side=Right, r);
          switch (side) {
          | Before => cats([place_caret_after(caret, l), root_tile(bin), r])
          | After => cats([l, root_tile(bin), place_caret_before(r)])
          };
        };
      },
    );
  let l =
    e
    |> decorate(
         fun
         | OpHole => mk_OpHole(~has_caret?, ())
         | Var(x) => mk_text(~has_caret?, x)
         | Num(n) => mk_text(~has_caret?, string_of_int(n))
         | Paren(body) => mk_Paren(mk_term(info, body)),
         fun
         | (Lam(p), body) => {
             let l_p = Layout_pat.mk_term(TypeInfo_exp.lam_pat(info), p);
             let l_body = mk_term(TypeInfo_exp.lam_body(info, p), body);
             (mk_Lam(l_p), l_body);
           }
         | (Let(p, def), body) => {
             let l_p = Layout_pat.mk_term(TypeInfo_exp.let_pat(info));
             let l_def = mk_term(TypeInfo_exp.let_def(info, p), def);
             let l_body = mk_term(TypeInfo_exp.let_body(info, p, def), body);
             (mk_Let(l_p, l_def), l_body);
           },
         fun
         | (_, Ap(_)) => failwith("ap todo"),
         fun
         | (l, Plus, r) => {
             let l_l = mk_term(TypeInfo_exp.plus_l(info), l);
             let l_r = mk_term(TypeInfo_exp.plus_r(info), r);
             (l_l, mk_Plus(), l_r);
           }
         | (l, BinHole, r) => {
             let l_l = mk_term(TypeInfo_exp.binhole_l(info), l);
             let l_r = mk_term(TypeInfo_exp.binhole_r(info), r);
             (l_l, mk_BinHole, l_r);
           },
       );
  err_hole(TypeInfo_exp.has_err(info, e), l);
};

let mk_selection =
    (
      ~style: option(Decoration.Selection.style)=?,
      ~grouts: list(t) => t,
      selection,
    ) => {
  let mk_tessera = selected =>
    Layout_unsorted.mk_tessera(
      ~style=
        Decoration.Tessera.mk_style(~highlighted=true, ~raised=selected, ()),
    );
  let mk_tile = selected =>
    Layout_unsorted.mk_tile(
      ~style=
        Decoration.Tile.mk_style(
          ~highlighted=selected,
          ~show_children=!selected,
          ~raised=selected,
          ~sort=?selected ? None : Some(Exp),
          (),
        ),
    );
  let selection =
    grouts(
      Selection.to_list(mk_tile(selected), mk_tessera(selected), selection),
    );
  switch (style) {
  | None => selection
  | Some(style) => Annot(Selection(style), selection)
  };
};

let mk_selecting =
    ((prefix, (side, selection), suffix): Subject.selecting(Tile_exp.t)) => {
  let prefix = mk_selection(~grouts=grouts_l, prefix);
  let selection =
    mk_selection(~grouts, ~style={transparent: false}, selection);
  let suffix = mk_selection(~grouts=grouts_r, suffix);
  let caret = caret(Selecting);
  let (prefix, selection) =
    switch (side) {
    | Left => (Cat(prefix, caret), selection)
    | Right => (prefix, Cat(selection, caret))
    };
  seps([prefix, selection, suffix]);
};

let mk_restructuring =
    ((prefix, selections, suffix): Subject.restructuring(Tile_exp.t)) => {
  let picked_up_all_selections = {
    let whole_prefix = OptUtil.sequence(List.map(Either.get_L, prefix))
    and whole_suffix = OptUtil.sequence(List.map(Either.get_L, suffix));
    Option.is_some(whole_prefix) && Option.is_some(whole_suffix);
  };
  let mk_affix =
    List.map(
      fun
      | Either.L(tile) =>
        Layout_unsorted.mk_tile(
          ~style=
            Decoration.Tile.mk_style(
              ~show_children=picked_up_all_selections,
              ~sort=Exp,
              (),
            ),
          tile,
        )
      | R(selection) =>
        mk_selection(~style={transparent: false}, selection),
    );
  let caret = {
    let mk_tranparent_selections =
      List.map(mk_selection(~style={transparent: true}));
    let (before, selection, after) = selections;
    Restructuring((
      mk_transparent_selections(before),
      mk_selection(~style={transparent: false}, selection),
      mk_transparent_selections(after),
    ));
  };
  grout_z(mk_affix(prefix), caret, mk_affix(suffix));
};
