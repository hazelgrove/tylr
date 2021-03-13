type t = {
  ctx: Ctx.t,
  mode,
}
and mode =
  | Syn
  | Ana(Type.t)
  | Fn_pos;
