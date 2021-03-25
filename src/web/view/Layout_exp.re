open Util;
open Core;
open Layout;

let decorate_term =
  decorate_term(
    ~sort=Exp,
    ~is_op_hole=Term_exp.is_op_hole,
    ~is_bin_hole=Term_exp.is_bin_hole,
  );

let rec mk_term =
        (
          ~has_caret: option(CaretPosition.t)=?,
          info: TypeInfo_exp.t,
          e: Term_exp.t,
        )
        : Layout.t => {
  open Term_exp;
  let has_caret = Option.map(pos => (Pointing(Exp(info)), pos), has_caret);
  let l =
    e
    |> decorate_term(
         ~has_caret,
         ~type_info=Exp(info),
         fun
         | OpHole => mk_OpHole(~has_caret?, ())
         | Var(x) => mk_text(~has_caret?, x)
         | Num(n) => mk_text(~has_caret?, string_of_int(n))
         | Paren(body) => mk_Paren(~has_caret?, mk_term(info, body)),
         fun
         | (Lam(p), body) => {
             let l_p = Layout_pat.mk_term(TypeInfo_exp.lam_pat(info), p);
             let l_body = mk_term(TypeInfo_exp.lam_body(p, info), body);
             (mk_Lam(~has_caret?, l_p), l_body);
           }
         | (Let(p, def), body) => {
             let l_p = Layout_pat.mk_term(TypeInfo_exp.let_pat(info), p);
             let l_def = mk_term(TypeInfo_exp.let_def(p, info), def);
             let l_body = mk_term(TypeInfo_exp.let_body(p, def, info), body);
             (mk_Let(~has_caret?, l_p, l_def), l_body);
           },
         fun
         | (fn, Ap(arg)) => {
             let _l_fn = mk_term(TypeInfo_exp.ap_fn(info), fn);
             let _l_arg = mk_term(TypeInfo_exp.ap_arg(fn, info), arg);
             failwith("ap todo");
           },
         fun
         | (l, Plus, r) => {
             let l_l = mk_term(TypeInfo_exp.plus_l(info), l);
             let l_r = mk_term(TypeInfo_exp.plus_r(info), r);
             (l_l, mk_Plus(~has_caret?, ()), l_r);
           }
         | (l, BinHole, r) => {
             let l_l = mk_term(TypeInfo_exp.binhole_l(info), l);
             let l_r = mk_term(TypeInfo_exp.binhole_r(info), r);
             (l_l, mk_BinHole(~has_caret?, ()), l_r);
           },
       );
  TypeInfo_exp.has_err(info, e)
    ? Annot(ErrHole(Option.is_some(has_caret)), l) : l;
};

let rec mk_frame =
        (~show_err_holes: bool, frame: Frame_exp.t)
        : TypeInfo_exp.t'(Layout.frame) =>
  switch (frame) {
  | Uni(uni) => mk_uniframe(~show_err_holes, uni)
  | Bi(bi) => mk_biframe(~show_err_holes, bi)
  }
and mk_uniframe = (~show_err_holes, uni: Frame_exp.unidelimited) =>
  switch (uni) {
  | Pre_r(Lam(p), frame) =>
    let info_lam = mk_frame(~show_err_holes, frame);
    let info_p = TypeInfo_exp.(lam_pat(of_t'(info_lam)));
    let l_p = Layout_pat.mk_term(info_p, p);
    let l_lam = l_body => cat(grouts_l([fst(mk_Lam(l_p))]), l_body);
    TypeInfo_exp.lam_body'(
      (l_frame, l_body) => {
        let l_lam =
          TypeInfo_exp.lam_has_err(info_lam)
            ? l_lam(l_body) : Annot(ErrHole(true), l_lam(l_body));
        l_frame(l_lam);
      },
      p,
      info_lam,
    );
  | Pre_r(Let(p, def), frame) =>
    let info = mk_frame(~show_err_holes, frame);
    let info_p = TypeInfo_pat.{ctx: info.ctx, mode: syn};
    let l_p = Layout_pat.mk_term(info_p, p);
    let l_def = {
      let (p_ty, _) = TypeInfo_pat.synthesize(info_p, p);
      let info_def = TypeInfo_exp.{ctx: info.ctx, mode: Ana(p_ty, ())};
      mk_term(info_def, def);
    };
    let ctx_body = TypeInfo_exp.extend_ctx_let_body(p, def, info.ctx);
    let mode_body =
      info.mode
      |> TypeInfo_exp.map_mode((l_frame, l_body) =>
           l_frame(cat(grouts_l([fst(mk_Let(l_p, l_def))]), l_body))
         );
    {ctx: ctx_body, mode: mode_body};
  | Post_l(_, Ap(_)) => failwith("ap todo")

  // TODO extract shared logic in bin cases
  | Bin_l(frame, Plus, r) =>
    let info = mk_frame(~show_err_holes, frame);
    let l_r = mk_term({ctx: info.ctx, mode: Ana(Num, ())}, r);
    let l_plus = l_l => cats([l_l, fst(mk_Plus()), l_r]);
    let mode_l: TypeInfo_exp.mode(_) =
      switch (info.mode) {
      | Syn(l_frame) => Ana(Num, l_l => l_frame(Num, l_plus(l_l)))
      | Ana(ty, l_frame) =>
        Ana(
          Num,
          l_l =>
            err_hole(
              show_err_holes && !Type.consistent(ty, Num),
              true,
              l_frame(l_plus(l_l)),
            ),
        )
      | Fn_pos(l_frame) =>
        Ana(
          Num,
          l_l =>
            err_hole(
              show_err_holes,
              true,
              l_frame(Hole, Hole, l_plus(l_l)),
            ),
        )
      };
    {...info, mode: mode_l};
  | Bin_r(l, Plus, frame) =>
    let info = mk_frame(~show_err_holes, frame);
    let l_l = mk_term({ctx: info.ctx, mode: Ana(Num, ())}, l);
    let l_plus = l_r => cats([l_l, fst(mk_Plus()), l_r]);
    let mode_r: TypeInfo_exp.mode(_) =
      switch (info.mode) {
      | Syn(l_frame) => Ana(Num, l_r => l_frame(Num, l_plus(l_r)))
      | Ana(ty, l_frame) =>
        Ana(
          Num,
          l_r =>
            err_hole(
              show_err_holes && !Type.consistent(ty, Num),
              true,
              l_frame(l_plus(l_r)),
            ),
        )
      | Fn_pos(l_frame) =>
        Ana(
          Num,
          l_r =>
            err_hole(
              show_err_holes,
              true,
              l_frame(Hole, Hole, l_plus(l_r)),
            ),
        )
      };
    {...info, mode: mode_r};

  | Bin_l(frame, BinHole, r) =>
    let info = mk_frame(~show_err_holes, frame);
    let l_r = mk_term({ctx: info.ctx, mode: TypeInfo_exp.syn}, r);
    let l_binhole = l_l => cats([l_l, empty_hole(fst(mk_BinHole())), l_r]);
    let mode_l: TypeInfo_exp.mode(_) =
      switch (info.mode) {
      | Syn(l_frame) => Syn((_, l_l) => l_frame(Hole, l_binhole(l_l)))
      | Ana(_, l_frame) => Syn((_, l_l) => l_frame(l_binhole(l_l)))
      | Fn_pos(l_frame) =>
        Syn((_, l_l) => l_frame(Hole, Hole, l_binhole(l_l)))
      };
    {...info, mode: mode_l};
  | Bin_r(l, BinHole, frame) =>
    let info = mk_frame(~show_err_holes, frame);
    let l_l = mk_term({ctx: info.ctx, mode: TypeInfo_exp.syn}, l);
    let l_binhole = l_r => cats([l_l, empty_hole(fst(mk_BinHole())), l_r]);
    let mode_l: TypeInfo_exp.mode(_) =
      switch (info.mode) {
      | Syn(l_frame) => Syn((_, l_r) => l_frame(Hole, l_binhole(l_r)))
      | Ana(_, l_frame) => Syn((_, l_r) => l_frame(l_binhole(l_r)))
      | Fn_pos(l_frame) =>
        Syn((_, l_r) => l_frame(Hole, Hole, l_binhole(l_r)))
      };
    {...info, mode: mode_l};
  }
and mk_biframe = (~show_err_holes, bi: Frame_exp.bidelimited) =>
  switch (bi) {
  | Root => TypeInfo_exp.{ctx: Ctx.empty, mode: Syn((_, l) => l)}
  | Closed () => raise(Frame_exp.Void_closed)
  | Open(Paren_body(frame)) =>
    let info = mk_frame(~show_err_holes, frame);
    {
      ...info,
      mode:
        TypeInfo_exp.map_mode(
          (l_frame, l_body) => l_frame(grouts([fst(mk_Paren(l_body))])),
          info.mode,
        ),
    };
  | Open(_) => failwith("go_bidelimited todo")
  };

// TODO clean up function args
let mk_selection =
    (
      ~style: option(Layout.selection_style)=?,
      ~selected: bool,
      ~grouts: list(t) => t,
      selection,
    ) => {
  let mk_tessera =
    Layout_unsorted.mk_tessera(
      ~style=
        Layout.mk_tessera_style(~highlighted=selected, ~raised=selected, ()),
    );
  let mk_tile =
    Layout_unsorted.mk_tile(
      ~style=
        Layout.mk_tile_style(
          ~highlighted=selected,
          ~show_children=true,
          ~raised=selected,
          ~sort=?selected ? None : Some(Exp),
          (),
        ),
    );
  let selection = grouts(Selection.to_list(mk_tile, mk_tessera, selection));
  switch (style) {
  | None => selection
  | Some(style) => Annot(Selection(style), selection)
  };
};

let mk_selecting =
    (((side, selection), (prefix, suffix)): Subject.selecting(Tile_exp.t)) => {
  let prefix =
    List.rev(prefix)
    |> Selection.map_tile(Parser_exp.unsort)
    |> mk_selection(~grouts=grouts_l, ~selected=false);
  let suffix =
    suffix
    |> Selection.map_tile(Parser_exp.unsort)
    |> mk_selection(~grouts=grouts_r, ~selected=false);
  let selection =
    selection
    |> mk_selection(~grouts, ~style={unfocused: false}, ~selected=true)
    |> place_caret(side, Selecting);
  cats([prefix, selection, suffix]);
};

let mk_restructuring =
    (
      ((selection, selections), (prefix, suffix)):
        Subject.restructuring(Tile_exp.t),
    ) => {
  let picked_up_all_selections = {
    let whole_prefix = OptUtil.sequence(List.map(Either.get_L, prefix))
    and whole_suffix = OptUtil.sequence(List.map(Either.get_L, suffix));
    Option.is_some(whole_prefix) && Option.is_some(whole_suffix);
  };
  let (prefix, suffix) =
    (List.rev(prefix), suffix)
    |> TupleUtil.map2(
         List.map(
           fun
           | Either.L(tile) =>
             tile
             |> Parser_exp.unsort
             |> Layout_unsorted.mk_tile(
                  ~style=
                    Layout.mk_tile_style(
                      ~show_children=picked_up_all_selections,
                      ~sort=Exp,
                      (),
                    ),
                )
           | R(selection) =>
             mk_selection(
               ~style={unfocused: false},
               ~grouts=grouts_inner,
               ~selected=false,
               selection,
             ),
         ),
       );
  let caret = {
    let mk_unfocused_selections =
      List.map(
        mk_selection(~style={unfocused: true}, ~grouts, ~selected=true),
      );
    let (before, after) = selections;
    Restructuring(
      mk_selection(
        ~style={unfocused: false},
        ~grouts,
        ~selected=true,
        selection,
      ),
      (mk_unfocused_selections(before), mk_unfocused_selections(after)),
    );
  };
  grouts_z(prefix, caret, suffix);
};
