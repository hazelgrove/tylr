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

let synthesize = (_, _) => failwith("todo TypeInfo_pat.synthesize");
