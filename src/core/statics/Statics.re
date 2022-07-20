open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type info_exp = {
  inherent: Typ.inherent,
  mode: Typ.mode,
  ctx: Typ.ctx,
  co_ctx: Typ.co_ctx,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type info_pat = unit; //TODO(andrew)

[@deriving (show({with_path: false}), sexp, yojson)]
type info_typ = unit;

[@deriving (show({with_path: false}), sexp, yojson)]
type info =
  | Invalid
  | InfoExp(info_exp)
  | InfoPat(info_pat)
  | InfoTyp(info_typ);

type info_map = Id.Map.t(info);

let union_co_ctxs = (co_ctx1, _co_ctx2) => co_ctx1;
//Id.Map.union((_, a, b) => Some(a), co_ctx1, co_ctx2);
//TODO(andrew): do this
let union_ms = (m1, m2) => Id.Map.union((_, _, b) => Some(b), m1, m2);

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
  | OpInt(Plus, uexp1, uexp2) =>
    binop(uexp1, uexp2, Ana(Int), Ana(Int), Just(Int))
  | OpInt(Lt, uexp1, uexp2) =>
    binop(uexp1, uexp2, Ana(Int), Ana(Int), Just(Bool))
  | OpBool(And, uexp1, uexp2) =>
    binop(uexp1, uexp2, Ana(Bool), Ana(Bool), Just(Bool))
  | Fun(_upat, _uexp) => (Unknown, [], addm(Invalid))
  | FunAnn(_upat, _utyp, _uexp) => (Unknown, [], addm(Invalid))
  | Let(_upat, _uexp, _uexp') => (Unknown, [], addm(Invalid))
  | LetAnn(_upat, _utyp, _uexp, _uexp') => (Unknown, [], addm(Invalid))
  | If(_uexp, _uexp', _uexp'') => (Unknown, [], addm(Invalid))
  | Ap(_uexp, _uexp') => (Unknown, [], addm(Invalid))
  };
};

let upat_to_info_map =
    (
      ~m=Id.Map.empty,
      ~ctx as _=VarMap.empty,
      ~mode as _=Typ.Syn,
      {id: _, term_p}: Term.upat,
    )
    : info_map => {
  //TODO(andrew): implement
  switch (term_p) {
  | InvalidPat(_p) => m
  | EmptyHolePat => m
  | Wild => m
  | IntPat(_i) => m
  | BoolPat(_b) => m
  | VarPat(_name) => m
  };
};

let utyp_to_info_map =
    (
      ~m=Id.Map.empty,
      ~ctx as _=VarMap.empty,
      ~mode as _=Typ.Syn,
      {id: _, term_t}: Term.utyp,
    )
    : info_map => {
  //TODO(andrew): implement
  switch (term_t) {
  | InvalidTyp(_p) => m
  | EmptyHoleTyp => m
  | Int => m
  | Bool => m
  | Arrow(_ty, _ty') => m
  };
};
