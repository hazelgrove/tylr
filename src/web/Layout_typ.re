open Core;
open Layout;

let decorate_term =
  decorate_term(
    ~sort=Typ,
    ~type_info=Typ,
    ~is_op_hole=Term_typ.is_op_hole,
    ~is_bin_hole=Term_typ.is_bin_hole,
  );

let rec mk_term = (~has_caret: option(CaretPosition.t)=?, ty: Term_typ.t) => {
  open Term_typ;
  let has_caret = Option.map(pos => (Pointing(Typ), pos), has_caret);
  ty
  |> decorate_term(
       ~has_caret,
       fun
       | OpHole => mk_OpHole(~has_caret?, ())
       | Num => mk_text(~has_caret?, "num")
       | Bool => mk_text(~has_caret?, "bool")
       | Paren(body) => mk_Paren(~has_caret?, mk_term(body)),
       (((), _)) => raise(Void_pre),
       ((_, ())) => raise(Void_post),
       fun
       | (l, BinHole, r) => (
           mk_term(l),
           mk_BinHole(~has_caret?, ()),
           mk_term(r),
         )
       | (l, Arrow, r) => (
           mk_term(l),
           mk_Arrow(~has_caret?, ()),
           mk_term(r),
         ),
     );
};
