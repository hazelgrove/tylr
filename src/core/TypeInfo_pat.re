type t'('a) = {
  ctx: Ctx.t,
  mode: mode('a),
}
and mode('a) =
  | Syn((Type.t, Ctx.t) => 'a)
  | Ana(Type.t, Ctx.t => 'a);

type t = t'(unit);
let syn = Syn((_, _) => ());
let ana = ty => Ana(ty, _ => ());

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
               };
             (Hole, Ctx.add(x, ty_ctx, ctx));
           }
         | Paren(body) => synthesize(info, body),
         (((), _)) => raise(Term_pat.Void_pre),
         fun
         | (subj, Ann(ann)) => {
             let mode = ana(Term_typ.to_type(ann));
             synthesize({mode, ctx: info.ctx}, subj);
           },
         fun
         | (l, BinHole, r) => {
             let (_, ctx) = synthesize({ctx: info.ctx, mode: syn}, l);
             let (_, ctx) = synthesize({ctx, mode: syn}, r);
             (Type.Hole, ctx);
           },
       )
  );
