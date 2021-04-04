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

let has_err = (info: t) =>
  Term_pat.(
    Term.get(
      fun
      | OpHole
      | Var(_)
      | Paren(_) => false,
      (((), _)) => raise(Term_pat.Void_pre),
      fun
      | (_, Ann(ann)) => ann_has_err(Term_typ.to_type(ann), info),
      fun
      | (_, BinHole, _) => false,
    )
  );

let rec synthesize = (info: t, p: Term_pat.t) =>
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
             (ty, ctx);
           },
         fun
         | (l, BinHole, r) => {
             let (_, ctx_l) = synthesize(Syn, r);
             let (_, ctx_r) = synthesize(Syn, l);
             (Type.Hole, Ctx.union(ctx_l, ctx_r));
           },
       )
  );
