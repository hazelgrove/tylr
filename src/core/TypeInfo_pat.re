// open Sexplib.Std;

[@deriving sexp]
type mode =
  | Syn
  | Ana(Type.t)
  | Let_pat(Type.t /* p ty */ => Type.t /* consistent def ty */);

[@deriving sexp]
type t = mode;

let root = Syn;

type out('a) = (Type.t, Ctx.t) => 'a;

let ann_has_err = (ty_ann: Type.t, info_ann: t): bool =>
  switch (info_ann) {
  | Syn
  | Let_pat(_) => false
  | Ana(ty) => !Type.consistent(ty_ann, ty)
  };

let prod_l = (info_prod: t) =>
  switch (info_prod) {
  | Syn
  // TODO need to review how let_pat works
  | Let_pat(_) => Syn
  | Ana(ty) =>
    switch (Type.matches_prod(ty)) {
    | None => Syn
    | Some((ty_l, _)) => Ana(ty_l)
    }
  };
let prod_r = (info_prod: t) =>
  switch (info_prod) {
  | Syn
  // TODO need to review how let_pat works
  | Let_pat(_) => Syn
  | Ana(ty) =>
    switch (Type.matches_prod(ty)) {
    | None => Syn
    | Some((_, ty_r)) => Ana(ty_r)
    }
  };

let subsume = (info, ty) =>
  switch (info) {
  | Syn
  | Let_pat(_) => ty
  | Ana(ty') => Type.consistent(ty, ty') ? ty : Hole
  };

let has_err = (info: t) =>
  Term_pat.(
    Term.get(
      fun
      | OpHole
      | Var(_)
      | Paren(_) => false,
      (((), _)) => raise(Term_pat.Void_pre),
      fun
      | (_, Ann(ann)) =>
        switch (info) {
        | Ana(ty) => !Type.consistent(ty, Term_typ.to_type(ann))
        | Syn
        | Let_pat(_) => false
        },
      fun
      | (_, BinHole, _) => false
      | (_, Prod, _) =>
        switch (info) {
        | Ana(ty) => !Type.consistent(ty, Prod(Hole, Hole))
        | Syn
        | Let_pat(_) => false
        },
    )
  );

let rec synthesize = (info: t, p: Term_pat.t) => {
  let subsume = subsume(info);
  Term_pat.(
    p
    |> Term.get(
         fun
         | OpHole => (Type.Hole, Ctx.empty)
         | Var(x) => {
             let ty =
               switch (info) {
               | Syn => Type.Hole
               | Ana(ty) => ty
               | Let_pat(ty_def) => ty_def(Hole)
               };
             (Hole, Ctx.singleton(x, ty));
           }
         | Paren(body) => synthesize(info, body),
         (((), _)) => raise(Term_pat.Void_pre),
         fun
         | (subj, Ann(ann)) => {
             let ty = Term_typ.to_type(ann);
             let (_, ctx) = synthesize(Ana(ty), subj);
             (subsume(ty), ctx);
           },
         fun
         | (l, BinHole, r) => {
             let (_, ctx_l) = synthesize(Syn, r);
             let (_, ctx_r) = synthesize(Syn, l);
             (Type.Hole, Ctx.union(ctx_l, ctx_r));
           }
         | (l, Prod, r) => {
             let (ty_l, ctx_l) = synthesize(prod_l(info), l);
             let (ty_r, ctx_r) = synthesize(prod_r(info), r);
             (subsume(Prod(ty_l, ty_r)), Ctx.union(ctx_l, ctx_r));
           },
       )
  );
};
