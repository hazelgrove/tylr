open Util;
open Core;
open Layout;

let uni_child = uni_child(~sort=Exp);

let root_tile = (~has_caret, ~shape, l) =>
  switch (has_caret) {
  | None => l
  | Some(_) => root_tile(~sort=Exp, ~shape, l)
  };

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
        let is_op_hole = Term_exp.is_op_hole(op);
        let (op, dangling_caret) = f_op(op);
        let op = root_tile(~has_caret, ~shape=Op(is_op_hole), op);
        switch (dangling_caret) {
        | None => grouts([op])
        | Some(Direction.Left) => grouts_z([], caret, [op])
        | Some(Right) => grouts_z([op], caret, [])
        };
      },
      ((pre, r)) => {
        let ((pre, dangling_caret), r) = f_pre((pre, r));
        let pre = root_tile(~has_caret, ~shape=Pre(), pre);
        switch (dangling_caret) {
        | None => cat(grouts_l([pre]), r)
        | Some(side) =>
          let r = uni_child(~side=Right, r);
          switch ((side: Direction.t)) {
          | Direction.Left => cats([grout(~caret, ()), pre, r])
          | Right => cat(grouts_l([pre]), place_caret(Left, caret, r))
          };
        };
      },
      ((l, post)) => {
        let (l, (post, dangling_caret)) = f_post((l, post));
        let post = root_tile(~has_caret, ~shape=Post(), post);
        switch (dangling_caret) {
        | None => cat(l, grouts_r([post]))
        | Some(side) =>
          let l = uni_child(~side=Left, l);
          switch (side) {
          | Direction.Left =>
            cat(place_caret(Right, caret, l), grouts_r([post]))
          | Right => cats([l, post, grout(~caret, ())])
          };
        };
      },
      ((l, bin, r)) => {
        let is_bin_hole = Term_exp.is_bin_hole(bin);
        let (l, (bin, dangling_caret), r) = f_bin((l, bin, r));
        let bin = root_tile(~has_caret, ~shape=Bin(is_bin_hole), bin);
        switch (dangling_caret) {
        | None => cats([l, bin, r])
        | Some(side) =>
          let l = uni_child(~side=Left, l);
          let r = uni_child(~side=Right, r);
          switch (side) {
          | Direction.Left => cats([place_caret(Right, caret, l), bin, r])
          | Right => cats([l, bin, place_caret(Left, caret, r)])
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

let mk_frame =
    (~show_err_holes: bool, frame: Frame_exp.t)
    : TypeInfo_exp.t'(Layout.frame) => {
  let rec go = (frame: Frame_exp.t) =>
    switch (frame) {
    | Uni(uni) => go_unidelimited(uni)
    | Bi(bi) => go_bidelimited(bi)
    }
  and go_unidelimited = (uni: Frame_exp.unidelimited) =>
    switch (uni) {
    | Pre_r(Lam(p), frame) =>
      let info: TypeInfo_exp.t'(Layout.frame) = go(frame);
      let info_p = TypeInfo_exp.(lam_pat(of_t'(info)));
      let l_p = Layout_pat.mk_term(info_p, p);
      let (ty_p, ctx_body) = TypeInfo_pat.synthesize(info_p, p);
      let mode_body: TypeInfo_exp.mode(_) = {
        let l_lam = l_body => cat(grouts_l([fst(mk_Lam(l_p))]), l_body);
        switch (info.mode) {
        | Syn(l_frame) =>
          Syn((ty_body, l_body) => l_frame(ty_body, l_lam(l_body)))
        | Fn_pos(l_frame) =>
          Syn((ty_body, l_body) => l_frame(ty_p, ty_body, l_lam(l_body)))
        | Ana(ty, l_frame) =>
          switch (Type.matches_arrow(ty)) {
          | None =>
            Syn(
              (_ty_body, l_body) =>
                l_frame(Annot(ErrHole(true), l_lam(l_body))),
            )
          | Some((_, ty_out)) => Ana(ty_out, l_frame)
          }
        };
      };
      {ctx: ctx_body, mode: mode_body};
    | Pre_r(Let(p, def), frame) =>
      let info = go(frame);
      let info_p = TypeInfo_pat.{ctx: info.ctx, mode: syn};
      let l_p = Layout_pat.mk_term(info_p, p);
      let l_def = {
        let (p_ty, _) = TypeInfo_pat.synthesize(info_p, p);
        let info_def = TypeInfo_exp.{ctx: info.ctx, mode: Ana(p_ty, ())};
        mk_term(info_def, def);
      };
      let ctx_body = TypeInfo_exp.extend_ctx_let_body(info.ctx, p, def);
      let mode_body =
        info.mode
        |> TypeInfo_exp.map_mode((l_frame, l_body) =>
             l_frame(cat(grouts_l([fst(mk_Let(l_p, l_def))]), l_body))
           );
      {ctx: ctx_body, mode: mode_body};
    | Post_l(_, Ap(_)) => failwith("ap todo")
    | Bin_l(frame, Plus, r) =>
      let info = go(frame);
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
      let info = go(frame);
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
    | Bin_l(_, BinHole, _)
    | Bin_r(_, BinHole, _) => failwith("binhole todo")
    }
  and go_bidelimited = (bi: Frame_exp.bidelimited) =>
    switch (bi) {
    | Root => TypeInfo_exp.{ctx: Ctx.empty, mode: Syn((_, l) => l)}
    | Closed () => raise(Frame_exp.Void_closed)
    | Open(Paren_body(frame)) =>
      let info = go(frame);
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
  go(frame);
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
      ~style=Layout.mk_tessera_style(~highlighted=true, ~raised=selected, ()),
    );
  let mk_tile =
    Layout_unsorted.mk_tile(
      ~style=
        Layout.mk_tile_style(
          ~highlighted=selected,
          ~show_children=!selected,
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
    ((prefix, (side, selection), suffix): Subject.selecting(Tile_exp.t)) => {
  let (prefix, suffix) =
    (prefix, suffix)
    |> TupleUtil.map2(Selection.map_tile(Parser_exp.unsort))
    |> TupleUtil.map2(mk_selection(~grouts=grouts_l, ~selected=false));
  let selection =
    selection
    |> mk_selection(~grouts, ~style={transparent: false}, ~selected=true)
    |> place_caret(side, Selecting);
  cats([prefix, selection, suffix]);
};

let mk_restructuring =
    ((prefix, selections, suffix): Subject.restructuring(Tile_exp.t)) => {
  let picked_up_all_selections = {
    let whole_prefix = OptUtil.sequence(List.map(Either.get_L, prefix))
    and whole_suffix = OptUtil.sequence(List.map(Either.get_L, suffix));
    Option.is_some(whole_prefix) && Option.is_some(whole_suffix);
  };
  let (prefix, suffix) =
    (prefix, suffix)
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
               ~style={transparent: false},
               ~grouts,
               ~selected=true,
               selection,
             ),
         ),
       );
  let caret = {
    let mk_transparent_selections =
      List.map(
        mk_selection(~style={transparent: true}, ~grouts, ~selected=true),
      );
    let (before, selection, after) = selections;
    Restructuring((
      mk_transparent_selections(before),
      mk_selection(
        ~style={transparent: false},
        ~grouts,
        ~selected=true,
        selection,
      ),
      mk_transparent_selections(after),
    ));
  };
  grouts_z(prefix, caret, suffix);
};
