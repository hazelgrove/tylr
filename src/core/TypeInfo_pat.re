open Sexplib.Std;

[@deriving sexp]
type t'('a) = {
  ctx: Ctx.t,
  mode: mode('a),
}
and mode('a) =
  | Syn((Type.t, Ctx.t) => 'a)
  | Ana(Type.t /* expected ty */, (Type.t /* consistent ty */, Ctx.t) => 'a)
  | Let_pat(
      Type.t /* p ty */ => Type.t /* consistent def ty */,
      (Type.t /* p ty */, Ctx.t /* ctx body */) => 'a,
    );

[@deriving sexp]
type t = t'(unit);
let syn = Syn((_, _) => ());
let ana = ty => Ana(ty, (_, _) => ());

let has_err = (info: t'(_)) =>
  Term_pat.(
    Term.get(
      fun
      | OpHole
      | Var(_)
      | Paren(_) => false,
      (((), _)) => raise(Term_pat.Void_pre),
      fun
      | (_, Ann(ann)) =>
        switch (info.mode) {
        | Ana(ty, _) => !Type.consistent(ty, Term_typ.to_type(ann))
        | Syn(_)
        | Let_pat(_) => false
        },
      fun
      | (_, BinHole, _) => false,
    )
  );

let rec synthesize = ({ctx, mode} as info: t, p: Term_pat.t) =>
  Term_pat.(
    p
    |> Term.get(
         fun
         | OpHole => (Type.Hole, ctx)
         | Var(x) => {
             let ty_ctx =
               switch (mode) {
               | Syn(_) => Type.Hole
               | Ana(ty, _) => ty
               | Let_pat(ty_def, _) => ty_def(Hole)
               };
             (Hole, Ctx.add(x, ty_ctx, ctx));
           }
         | Paren(body) => synthesize(info, body),
         (((), _)) => raise(Term_pat.Void_pre),
         fun
         | (subj, Ann(ann)) => {
             let ty = Term_typ.to_type(ann);
             let (_, ctx) =
               synthesize({mode: ana(ty), ctx: info.ctx}, subj);
             (ty, ctx);
           },
         fun
         | (l, BinHole, r) => {
             let (_, ctx) = synthesize({ctx: info.ctx, mode: syn}, l);
             let (_, ctx) = synthesize({ctx, mode: syn}, r);
             (Type.Hole, ctx);
           },
       )
  );

let ann_subj = (ann: Term_typ.t, info_ann: t'(_)): t => {
  let ty = Term_typ.to_type(ann);
  {ctx: info_ann.ctx, mode: ana(ty)};
};

let binhole_l = (info_binhole: t'(_)): t => {...info_binhole, mode: syn};
let binhole_r = binhole_l;
