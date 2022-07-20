//open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type info_exp = {
  inherent: Typ.inherent,
  mode: Typ.mode,
  ctx: Typ.ctx,
  co_ctx: Typ.co_ctx,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type info_pat = {inherent: Typ.inherent}; //TODO(andrew): more

[@deriving (show({with_path: false}), sexp, yojson)]
type info_typ = {ty: Typ.t};

[@deriving (show({with_path: false}), sexp, yojson)]
type info =
  | Invalid
  | InfoExp(info_exp)
  | InfoPat(info_pat)
  | InfoTyp(info_typ);

type info_map = Id.Map.t(info);

let union_co_ctxs = (co_ctx1, _co_ctx2) => co_ctx1; //TODO(andrew): write this

let union_co_ctxs_all = List.fold_left(union_co_ctxs, []);
let union_ms = (m1, m2) => Id.Map.union((_, _, b) => Some(b), m1, m2);
let union_m_all = List.fold_left(union_ms, Id.Map.empty);

let ty_of_inherent: Typ.inherent => Typ.t =
  fun
  | Just(t) => t
  | Joined(_ts) => Unknown //TODO(andrew): join?
  | Free => Unknown;

let rec uexp_to_info_map =
        (
          ~m=Id.Map.empty,
          ~ctx=VarMap.empty,
          ~mode=Typ.Syn,
          {id, term}: Term.uexp,
        )
        : (Typ.t, Typ.co_ctx, info_map) => {
  let go = uexp_to_info_map(~m, ~ctx);
  let addm = i => Id.Map.add(id, i, m);
  let addi' = (~inherent, ~co_ctx, m) => (
    ty_of_inherent(inherent),
    co_ctx,
    Id.Map.add(id, InfoExp({inherent, mode, ctx, co_ctx}), m),
  );
  let addi = (~inherent, ~co_ctx) => (
    ty_of_inherent(inherent),
    co_ctx,
    addm(InfoExp({inherent, mode, ctx, co_ctx})),
  );
  let atomic = inherent => addi(~inherent, ~co_ctx=[]);
  let binop = (uexp1, uexp2, ty_l, ty_r, ty_out) => {
    let (_ty1, co_ctx1, m1) = go(~mode=ty_l, uexp1);
    let (_ty2, co_ctx2, m2) = go(~mode=ty_r, uexp2);
    addi'(
      ~inherent=ty_out,
      ~co_ctx=union_co_ctxs(co_ctx1, co_ctx2),
      union_ms(m1, m2),
    );
  };
  //TODO(andrew): implement
  switch (term) {
  | InvalidExp(_p) => (Unknown, [], addm(Invalid))
  | EmptyHole => atomic(Free)
  | Bool(_b) => atomic(Just(Bool))
  | Int(_i) => atomic(Just(Int))
  | Var(name) =>
    switch (VarMap.lookup(ctx, name)) {
    | None => atomic(Free)
    | Some(ce) =>
      addi'(~inherent=Just(ce.typ), ~co_ctx=[(name, [{id, mode}])], m)
    }
  | OpInt(Plus, e1, e2) => binop(e1, e2, Ana(Int), Ana(Int), Just(Int))
  | OpInt(Lt, e1, e2) => binop(e1, e2, Ana(Int), Ana(Int), Just(Bool))
  | OpBool(And, e1, e2) => binop(e1, e2, Ana(Bool), Ana(Bool), Just(Bool))
  | If(cond, e1, e2) =>
    let (_, co_ctx_e0, m1) = go(~mode=Ana(Bool), cond);
    let (ty_e1, co_ctx_e1, m2) = go(~mode, e1);
    let (ty_e2, co_ctx_e2, m3) = go(~mode, e2);
    addi'(
      ~inherent=Joined([ty_e1, ty_e2]),
      ~co_ctx=union_co_ctxs_all([co_ctx_e0, co_ctx_e1, co_ctx_e2]),
      union_m_all([m1, m2, m3]),
    );
  | Fun(pat, body) =>
    let (ty_pat, ctx_pat, m_pat) = upat_to_info_map(~m, pat);
    let ctx = VarMap.union(ctx, ctx_pat);
    let (ty_body, co_ctx_body, m_body) =
      uexp_to_info_map(~m, ~ctx, ~mode, body);
    addi'(
      ~inherent=Just(Arrow(ty_pat, ty_body)),
      ~co_ctx=co_ctx_body, //TODO(andrew): remove current var uses
      union_m_all([m_pat, m_body]),
    );
  | FunAnn(_upat, _utyp, _uexp) => (Unknown, [], addm(Invalid))
  | Ap(_uexp, _uexp') => (Unknown, [], addm(Invalid))
  | Let(_upat, _uexp, _uexp') => (Unknown, [], addm(Invalid))
  | LetAnn(_upat, _utyp, _uexp, _uexp') => (Unknown, [], addm(Invalid))
  };
}
and upat_to_info_map =
    (
      ~m=Id.Map.empty,
      //~ctx as _=VarMap.empty,
      //~mode as _=Typ.Syn,
      {id, term_p}: Term.upat,
    )
    : (Typ.t, Typ.ctx, info_map) => {
  let addih = (~inherent: Typ.inherent, m) => (
    ty_of_inherent(inherent),
    [],
    Id.Map.add(id, InfoPat({inherent: inherent}), m),
  );
  let add = i => addih(~inherent=i, m);
  //TODO(andrew): implement
  switch (term_p) {
  | InvalidPat(_p) => add(Free)
  | EmptyHolePat => add(Free)
  | Wild => add(Just(Unknown))
  | IntPat(_) => add(Just(Int))
  | BoolPat(_) => add(Just(Bool))
  | VarPat(name) =>
    //TODO(andrew): Free doesnt make sense
    (
      Unknown,
      [(name, {id, typ: Unknown})],
      Id.Map.add(id, InfoPat({inherent: Free}), m),
    )
  };
}
and utyp_to_info_map =
    (
      ~m=Id.Map.empty,
      ~ctx as _=VarMap.empty,
      ~mode as _=Typ.Syn,
      {id, term_t} as utyp: Term.utyp,
    )
    : info_map => {
  let addm = i => Id.Map.add(id, i, m);
  //TODO(andrew): implement
  switch (term_t) {
  | InvalidTyp(_)
  | EmptyHoleTyp
  | Int
  | Bool
  | Arrow(_) => addm(InfoTyp({ty: Term.utyp_to_ty(utyp)}))
  };
};
