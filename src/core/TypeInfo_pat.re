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

let map_mode = (f: 'a => 'b) =>
  fun
  | Syn(a) => Syn((ty, ctx) => f(a(ty, ctx)))
  | Ana(ty, a) => Ana(ty, (ty', ctx) => f(a(ty', ctx)))
  | Let_pat(ty_def, a) => Let_pat(ty_def, (ty, ctx) => f(a(ty, ctx)));

[@deriving sexp]
type t = t'(unit);
let syn = Syn((_, _) => ());
let ana = ty => Ana(ty, (_, _) => ());

let of_t' = info => {...info, mode: map_mode(_ => (), info.mode)};

let root' = a => {ctx: Ctx.empty, mode: Syn(a)};

let ann_subj = (ann: Term_typ.t, info_ann: t'(_)): t => {
  let ty = Term_typ.to_type(ann);
  {ctx: info_ann.ctx, mode: ana(ty)};
};

let binhole_l = (info_binhole: t'(_)): t => {...info_binhole, mode: syn};

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
             let (_, ctx) = synthesize(ann_subj(ann, info), subj);
             (ty, ctx);
           },
         fun
         | (l, BinHole, r) => {
             let (_, ctx) = synthesize(binhole_r(l, info), r);
             (Type.Hole, ctx);
           },
       )
  )
and binhole_r = (l, info_binhole: t'(_)): t => {
  let (_, ctx) = synthesize({ctx: info_binhole.ctx, mode: syn}, l);
  {ctx, mode: syn};
};

let ann_subj' = (f: 'a => 'a, ann: Term_typ.t, info_ann: t'('a)): t'('a) => {
  let ty = Term_typ.to_type(ann);
  {
    ctx: info_ann.ctx,
    mode:
      Ana(
        ty,
        (_subj_ty, ctx) =>
          switch (info_ann.mode) {
          | Syn(a) => f(a(ty, ctx))
          | Ana(ty', a) =>
            let ty = Type.consistent(ty, ty') ? ty : Hole;
            f(a(ty, ctx));
          | Let_pat(_ty_def, a) =>
            // TODO resolve some inconsistency here relative
            // to elsewhere re: incorporating def ty
            f(a(ty, ctx))
          },
      ),
  };
};

let binhole_l' = (f: (Ctx.t, 'a) => 'b, r, info_binhole: t'('a)): t'('b) => {
  let mode =
    switch (info_binhole.mode) {
    | Syn(a)
    | Ana(_, a)
    | Let_pat(_, a) =>
      Syn(
        (_, ctx_l) => {
          let (_, ctx) = synthesize({ctx: ctx_l, mode: syn}, r);
          f(ctx_l, a(Hole, ctx));
        },
      )
    };
  {mode, ctx: info_binhole.ctx};
};
let binhole_r' = (f: 'a => 'b, l, info_binhole: t'('a)): t'('b) => {
  let mode =
    switch (info_binhole.mode) {
    | Syn(a)
    | Ana(_, a)
    | Let_pat(_, a) => Syn((_, ctx) => f(a(Hole, ctx)))
    };
  let (_, ctx) = synthesize({ctx: info_binhole.ctx, mode: syn}, l);
  {mode, ctx};
};
