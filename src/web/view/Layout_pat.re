open Core;
open Layout;

let decorate_term =
  decorate_term(
    ~sort=Pat,
    ~is_op_hole=Term_pat.is_op_hole,
    ~is_bin_hole=Term_pat.is_bin_hole,
  );

let rec mk_term =
        (
          ~has_caret: option(CaretPosition.t)=?,
          info: TypeInfo_pat.t,
          p: Term_pat.t,
        ) => {
  open Term_pat;
  let has_caret = Option.map(pos => (Pointing(Pat(info)), pos), has_caret);
  let l =
    p
    |> decorate_term(
         ~has_caret,
         ~type_info=Pat(info),
         fun
         | OpHole => mk_OpHole(~has_caret?, ())
         | Var(x) => mk_text(~has_caret?, x)
         | Paren(body) => mk_Paren(~has_caret?, mk_term(info, body)),
         (((), _)) => raise(Void_pre),
         fun
         | (subj, Ann(ann)) => {
             let l_subj = mk_term(TypeInfo_pat.ann_subj(ann, info), subj);
             let l_ann = Layout_typ.mk_term(ann);
             (l_subj, mk_Ann(~has_caret?, l_ann));
           },
         fun
         | (l, BinHole, r) => {
             let l_l = mk_term(TypeInfo_pat.binhole_l(info), l);
             let l_r = mk_term(TypeInfo_pat.binhole_r(info), r);
             (l_l, mk_BinHole(~has_caret?, ()), l_r);
           },
       );
  TypeInfo_pat.has_err(info, p)
    ? Annot(ErrHole(Option.is_some(has_caret)), l) : l;
};
