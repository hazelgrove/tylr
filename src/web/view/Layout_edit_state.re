open Util;
open Core;
open Layout;

module Exp = {
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
          Layout.mk_tessera_style(
            ~highlighted=selected,
            ~raised=selected,
            (),
          ),
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
    let selection =
      grouts(Selection.to_list(mk_tile, mk_tessera, selection));
    switch (style) {
    | None => selection
    | Some(style) => Annot(Selection(style), selection)
    };
  };

  let mk_selecting =
      (
        ((side, selection), (prefix, suffix)):
          Subject.selecting(Tile_exp.t),
      ) => {
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
};

let mk_pointing = (pointing: EditState_pointing.t) => {
  // TODO deduplicate shared logic across sorts
  let rec go =
          (~caret=CaretPosition.Before(0), pointing: EditState_pointing.t) =>
    switch (pointing) {
    | Typ(_) => failwith("todo Layout_edit_state.mk_pointing")
    | Pat(((prefix, [_, ..._] as suffix), frame)) =>
      let (term, (prefix, suffix)) = {
        let n = List.length(prefix);
        let tiles = ListUtil.of_frame((prefix, suffix));
        let skel = Skel.skel_at(n, Parser_pat.mk_skel(tiles));
        Parser_pat.term_of_skel(skel, tiles);
      };
      let frame = Parser_pat.associate_frame((prefix, suffix), frame);
      let l_frame = Layout_frame.Pat.mk(~show_err_holes=true, frame);
      let info_term = TypeInfo_pat.of_t'(l_frame);
      let l_term = Layout_term.Pat.mk(~has_caret=caret, info_term, term);
      switch (l_frame.mode) {
      | Syn(l_frame)
      | Ana(_, l_frame) =>
        let (ty, ctx) = TypeInfo_pat.synthesize(info_term, term);
        l_frame(ty, ctx, l_term);
      | Let_pat(_) => failwith("todo")
      };
    | Pat(_) => failwith("todo Layout_edit_state.mk_pointing")

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
      let l_frame = Layout_frame.Exp.mk(~show_err_holes=true, frame);
      let info_term = TypeInfo_exp.of_t'(l_frame);
      let l_term = Layout_term.Exp.mk(~has_caret=caret, info_term, term);
      switch (l_frame.mode) {
      | Syn(l_frame)
      | Ana(_, l_frame) =>
        let ty = TypeInfo_exp.synthesize(info_term, term);
        l_frame(ty, l_term);
      | Fn_pos(l_frame) =>
        let (ty_in, ty_out) =
          Option.get(
            Type.matches_arrow(TypeInfo_exp.synthesize(info_term, term)),
          );
        l_frame(ty_in, ty_out, l_term);
      };
    };
  go(pointing);
};

let mk_framed_subject = (l_subject, l_frame: TypeInfo_exp.t'(Layout.frame)) =>
  switch (l_frame.mode) {
  | Syn(l_frame)
  | Ana(_, l_frame) => l_frame(Hole, l_subject)
  | Fn_pos(l_frame) => l_frame(Hole, Hole, l_subject)
  };

let mk = (edit_state: EditState.t) =>
  switch (edit_state) {
  | Typ((Pointing(pointing), frame)) => mk_pointing(Typ((pointing, frame)))
  | Pat((Pointing(pointing), frame)) => mk_pointing(Pat((pointing, frame)))
  | Exp((Pointing(pointing), frame)) => mk_pointing(Exp((pointing, frame)))
  | Typ((Selecting(_) | Restructuring(_), _))
  | Pat((Selecting(_) | Restructuring(_), _)) =>
    failwith("todo Layout_edit_state.mk")
  | Exp((Selecting(selecting), frame)) =>
    let l_frame = Layout_frame.Exp.mk_bi(~show_err_holes=false, frame);
    let l_selecting = Exp.mk_selecting(selecting);
    mk_framed_subject(l_selecting, l_frame);
  | Exp((Restructuring(restructuring), frame)) =>
    let l_frame = Layout_frame.Exp.mk_bi(~show_err_holes=false, frame);
    let l_restructuring = Exp.mk_restructuring(restructuring);
    mk_framed_subject(l_restructuring, l_frame);
  };
