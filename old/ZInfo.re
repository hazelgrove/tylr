type t = {subject: UHExp.tile};

type t = {
  path: ZPath.t,
  ctx: Contexts.t,
  sort_info,
}
and sort_info =
  | Exp({
      mode: exp_mode,
      tile: UHExp.tile,
    })
  | Pat({
      mode: pat_mode,
      tile: UHPat.tile,
    })
  | Typ({
      mode: typ_mode,
      tile: UHTyp.tile,
    })
and exp_mode = unit
and pat_mode = unit
and typ_mode = unit;

let mk = (path: ZPath.t, e: UHExp.t): t => {};
