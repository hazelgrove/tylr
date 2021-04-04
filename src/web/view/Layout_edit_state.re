open Util;
open Core;
open Layout;

// TODO clean up function args
let mk_selection =
    (
      ~sort=?,
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
      ~sort?,
      ~style=
        Layout.mk_tile_style(
          ~highlighted=selected,
          ~show_children=true,
          ~raised=selected,
          ~sort=?selected ? None : sort,
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
    (
      ~unsort: 'tile => Unsorted.Tile.t,
      ~sort,
      ((side, selection), (prefix, suffix)): Subject.selecting('tile),
    ) => {
  let prefix =
    List.rev(prefix)
    |> Selection.map_tile(unsort)
    |> mk_selection(~sort, ~grouts=grouts_l, ~selected=false);
  let suffix =
    suffix
    |> Selection.map_tile(unsort)
    |> mk_selection(~sort, ~grouts=grouts_r, ~selected=false);
  let selection =
    selection
    |> mk_selection(~grouts, ~style={unfocused: false}, ~selected=true)
    |> place_caret(side, Selecting);
  cats([prefix, selection, suffix]);
};

let mk_restructuring =
    (
      ~sort,
      ~unsort: 'tile => Unsorted.Tile.t,
      ((selection, selections), (prefix, suffix)):
        Subject.restructuring('tile),
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
             |> unsort
             |> Layout_unsorted.mk_tile(
                  ~sort,
                  ~style=
                    Layout.mk_tile_style(
                      ~show_children=picked_up_all_selections,
                      ~sort,
                      (),
                    ),
                )
           | R(selection) =>
             mk_selection(~grouts=grouts_inner, ~selected=false, selection),
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

module Typ = {
  let mk_selecting = mk_selecting(~sort=Typ, ~unsort=Parser_typ.unsort);
  let mk_restructuring =
    mk_restructuring(~sort=Typ, ~unsort=Parser_typ.unsort);

  let mk_untyped_framed_subject = (l_subject, l_frame) =>
    // only called in non-pointing mode
    l_frame(Type.Hole, l_subject);
};

module Pat = {
  let mk_selecting = mk_selecting(~sort=Pat, ~unsort=Parser_pat.unsort);
  let mk_restructuring =
    mk_restructuring(~sort=Pat, ~unsort=Parser_pat.unsort);

  let mk_untyped_framed_subject =
      (l_subject, l_frame: (Type.t, Ctx.t) => Layout.frame) =>
    l_frame(Hole, Ctx.empty, l_subject);
};

module Exp = {
  let mk_selecting = mk_selecting(~sort=Exp, ~unsort=Parser_exp.unsort);
  let mk_restructuring =
    mk_restructuring(~sort=Exp, ~unsort=Parser_exp.unsort);

  let mk_untyped_framed_subject = (l_subject, l_frame: Type.t => Layout.frame) =>
    l_frame(Hole, l_subject);
};

let mk_pointing = (pointing: EditState_pointing.t) => {
  // TODO deduplicate shared logic across sorts
  let rec go =
          (~caret=CaretPosition.Before(0), pointing: EditState_pointing.t) =>
    switch (pointing) {
    | Typ(((prefix, []) as tiles, frame)) =>
      let subject = Parser_typ.associate(ListUtil.of_frame(tiles));
      switch (frame) {
      | Root =>
        let (first, trailing) = ListUtil.split_first(prefix);
        go(~caret=After, Typ(((trailing, [first]), frame)));
      | Open(Paren_body(frame)) =>
        let ((prefix, suffix), frame) = Parser_typ.dissociate_frame(frame);
        go(
          ~caret=Before(1),
          Typ(((prefix, [Op(Paren(subject)), ...suffix]), frame)),
        );
      | Closed(Ann_ann(subj, frame)) =>
        let inner_prefix = List.rev(Parser_pat.dissociate(subj));
        let ((prefix, suffix), frame) = Parser_pat.dissociate_frame(frame);
        go(
          ~caret=Before(1),
          Pat((
            (
              prefix @ inner_prefix,
              [Tile.Post(Term_pat.Ann(subject)), ...suffix],
            ),
            frame,
          )),
        );
      };
    | Typ(((prefix, [_, ..._] as suffix), frame)) =>
      let (term, (prefix, suffix)) = {
        let n = List.length(prefix);
        let tiles = ListUtil.of_frame((prefix, suffix));
        let skel = Skel.skel_at(n, Parser_typ.mk_skel(tiles));
        Parser_typ.term_of_skel(skel, tiles);
      };
      let frame = Parser_typ.associate_frame((prefix, suffix), frame);
      let l_frame = Layout_frame.Typ.mk(~show_err_holes=true, frame);
      let l_term = Layout_term.Typ.mk(~has_caret=caret, term);
      l_frame(Term_typ.to_type(term), l_term);

    | Pat(((prefix, []) as tiles, frame)) =>
      let subject = Parser_pat.associate(ListUtil.of_frame(tiles));
      switch (frame) {
      | Root =>
        let (first, trailing) = ListUtil.split_first(prefix);
        go(~caret=After, Pat(((trailing, [first]), frame)));
      | Open(Paren_body(frame)) =>
        let ((prefix, suffix), frame) = Parser_pat.dissociate_frame(frame);
        go(
          ~caret=Before(1),
          Pat(((prefix, [Op(Paren(subject)), ...suffix]), frame)),
        );
      | Closed(Lam_pat(frame, body)) =>
        let inner_suffix = Parser_exp.dissociate(body);
        let ((prefix, suffix), frame) = Parser_exp.dissociate_frame(frame);
        go(
          ~caret=Before(1),
          Exp((
            (
              prefix,
              [Tile.Pre(Term_exp.Lam(subject)), ...inner_suffix] @ suffix,
            ),
            frame,
          )),
        );
      | Closed(Let_pat(frame, def, body)) =>
        let inner_suffix = Parser_exp.dissociate(body);
        let ((prefix, suffix), frame) = Parser_exp.dissociate_frame(frame);
        go(
          ~caret=Before(1),
          Exp((
            (
              prefix,
              [Tile.Pre(Term_exp.Let(subject, def)), ...inner_suffix]
              @ suffix,
            ),
            frame,
          )),
        );
      };
    | Pat(((prefix, [_, ..._] as suffix), frame)) =>
      let (term, (prefix, suffix)) = {
        let n = List.length(prefix);
        let tiles = ListUtil.of_frame((prefix, suffix));
        let skel = Skel.skel_at(n, Parser_pat.mk_skel(tiles));
        Parser_pat.term_of_skel(skel, tiles);
      };
      let frame = Parser_pat.associate_frame((prefix, suffix), frame);
      let (info_term, l_frame) =
        Layout_frame.Pat.mk(~show_err_holes=true, frame);
      let l_term = Layout_term.Pat.mk(~has_caret=caret, info_term, term);
      switch (info_term) {
      | Syn
      | Ana(_) =>
        let (ty, ctx) = TypeInfo_pat.synthesize(info_term, term);
        l_frame(ty, ctx, l_term);
      | Let_pat(ty_def) =>
        let (ty_p, _) = TypeInfo_pat.synthesize(info_term, term);
        let (_, ctx_body) =
          TypeInfo_pat.synthesize(Ana(ty_def(ty_p)), term);

        l_frame(ty_p, ctx_body, l_term);
      };

    | Exp(((prefix, []), frame)) =>
      let subject = Parser_exp.associate(List.rev(prefix));
      switch (frame) {
      | Root =>
        let (first, trailing) = ListUtil.split_first(prefix);
        go(~caret=After, Exp(((trailing, [first]), frame)));
      | Open(Paren_body(frame)) =>
        let ((prefix, suffix), frame) = Parser_exp.dissociate_frame(frame);
        go(
          ~caret=Before(1),
          Exp(((prefix, [Op(Paren(subject)), ...suffix]), frame)),
        );
      | Open(Let_def(p, frame, body)) =>
        let inner_suffix = Parser_exp.dissociate(body);
        let ((prefix, suffix), frame) = Parser_exp.dissociate_frame(frame);
        go(
          ~caret=Before(2),
          Exp((
            (
              prefix,
              [Tile.Pre(Term_exp.Let(p, subject)), ...inner_suffix] @ suffix,
            ),
            frame,
          )),
        );
      | Open(Cond_then(_)) => failwith("cond todo")
      | Open(Ap_arg(_)) => failwith("ap todo")
      | Closed () => raise(Frame_exp.Void_closed)
      };
    | Exp(((prefix, [_, ..._] as suffix), frame)) =>
      let (term, (prefix, suffix)) = {
        let n = List.length(prefix);
        let tiles = ListUtil.of_frame((prefix, suffix));
        let skel = Skel.skel_at(n, Parser_exp.mk_skel(tiles));
        Parser_exp.term_of_skel(skel, tiles);
      };
      let frame = Parser_exp.associate_frame((prefix, suffix), frame);
      let (info_term, l_frame) =
        Layout_frame.Exp.mk(~show_err_holes=true, frame);
      let l_term = Layout_term.Exp.mk(~has_caret=caret, info_term, term);
      l_frame(TypeInfo_exp.synthesize(info_term, term), l_term);
    };
  go(pointing);
};

let mk = (edit_state: EditState.t) =>
  switch (edit_state) {
  | Typ((Pointing(pointing), frame)) => mk_pointing(Typ((pointing, frame)))
  | Pat((Pointing(pointing), frame)) => mk_pointing(Pat((pointing, frame)))
  | Exp((Pointing(pointing), frame)) => mk_pointing(Exp((pointing, frame)))

  | Typ((Selecting(selecting), frame)) =>
    let l_frame = Layout_frame.Typ.mk_bi(~show_err_holes=false, frame);
    let l_selecting = Typ.mk_selecting(selecting);
    Typ.mk_untyped_framed_subject(l_selecting, l_frame);
  | Pat((Selecting(selecting), frame)) =>
    let (_, l_frame) = Layout_frame.Pat.mk_bi(~show_err_holes=false, frame);
    let l_selecting = Pat.mk_selecting(selecting);
    Pat.mk_untyped_framed_subject(l_selecting, l_frame);
  | Exp((Selecting(selecting), frame)) =>
    let (_, l_frame) = Layout_frame.Exp.mk_bi(~show_err_holes=false, frame);
    let l_selecting = Exp.mk_selecting(selecting);
    Exp.mk_untyped_framed_subject(l_selecting, l_frame);

  | Typ((Restructuring(restructuring), frame)) =>
    let l_frame = Layout_frame.Typ.mk_bi(~show_err_holes=false, frame);
    let l_restructuring = Typ.mk_restructuring(restructuring);
    Typ.mk_untyped_framed_subject(l_restructuring, l_frame);
  | Pat((Restructuring(restructuring), frame)) =>
    let (_, l_frame) = Layout_frame.Pat.mk_bi(~show_err_holes=false, frame);
    let l_restructuring = Pat.mk_restructuring(restructuring);
    Pat.mk_untyped_framed_subject(l_restructuring, l_frame);
  | Exp((Restructuring(restructuring), frame)) =>
    let (_, l_frame) = Layout_frame.Exp.mk_bi(~show_err_holes=false, frame);
    let l_restructuring = Exp.mk_restructuring(restructuring);
    Exp.mk_untyped_framed_subject(l_restructuring, l_frame);
  };
