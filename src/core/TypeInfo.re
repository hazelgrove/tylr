type mode =
  | Syn
  | Ana(Type.t);

type t = {
  ctx: Ctx.t,
  mode,
};
