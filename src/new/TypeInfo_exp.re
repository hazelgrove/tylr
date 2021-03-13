type t'('a) = {
  ctx: Ctx.t,
  mode: mode('a),
}
and mode('a) =
  | Syn(Type.t => 'a)
  | Ana(Type.t, 'a)
  | Fn_pos((Type.t, Type.t) => 'a);

let map_mode = (f: 'a => 'b) =>
  fun
  | Syn(g) => Syn(ty => f(g(ty)))
  | Ana(ty, a) => Ana(ty, f(a))
  | Fn_pos(g) => Fn_pos((ty_in, ty_out) => f(g(ty_in, ty_out)));

type t = t'(unit);
let syn = Syn(_ => ());
let ana = ty => Ana(ty, ());
let fn_pos = Fn_pos((_, _) => ());

let of_t' = info => {...info, mode: map_mode(_ => (), info.mode)};

let lam_has_err = (info_lam: t) =>
  switch (info_lam.mode) {
  | Ana(ty, _) => Option.is_none(Type.matches_arrow(ty))
  | Syn(_)
  | Fn_pos(_) => false
  };
let lam_pat = (info_lam: t) => {
  let mode =
    switch (info_lam.mode) {
    | Syn(_)
    | Fn_pos(_) => TypeInfo_pat.syn
    | Ana(ty, _) =>
      switch (Type.matches_arrow(ty)) {
      | None => TypeInfo_pat.syn
      | Some((ty_in, _)) => TypeInfo_pat.ana(ty_in)
      }
    };
  TypeInfo_pat.{ctx: info_lam.ctx, mode};
};
let lam_body = (info_lam: t, p: Term_pat.t) => {
  let info_p = lam_pat(info_lam);
  let (_, ctx_body) = Statics_pat.syn(info_p, p);
  let mode_body =
    switch (info_lam.mode) {
    | Syn(_)
    | Fn_pos(_) => syn
    | Ana(ty, ()) =>
      switch (Type.matches_arrow(ty)) {
      | None => syn
      | Some((_, ty_out)) => ana(ty_out)
      }
    };
  {ctx: ctx_body, mode: mode_body};
};

let let_pat = (info_let: t) =>
  TypeInfo_pat.{ctx: info_let.ctx, mode: TypeInfo_pat.syn};
let let_def = (info_let: t, p: Term_pat.t) => {
  let (ty_p, _) = Statics_pat.syn(let_pat, p);
  {ctx: info_let.ctx, mode: ana(ty_p)};
};

let plus_has_err = (info_plus: t) =>
  switch (info_plus.mode) {
  | Syn(_) => false
  | Ana(ty, _) => !Type.consistent(ty, Num)
  | Fn_pos(_) => true
  };
let plus_l = (info_plus: t) => {...info_plus, mode: ana(Num)};
let plus_r = plus_l;

let binhole_l = (info_binhole: t) => {...info_binhole, mode: syn};
let binhole_r = binhole_l;
