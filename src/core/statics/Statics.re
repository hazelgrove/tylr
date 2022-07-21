//open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type info_exp = {
  cls: Term.UExp.cls,
  mode: Typ.mode,
  self: Typ.self,
  ctx: Typ.ctx,
  uses: Typ.co_ctx,
};

//TODO(andrew): ctx-like fields to detect duplicates
[@deriving (show({with_path: false}), sexp, yojson)]
type info_pat = {
  cls: Term.UPat.cls,
  mode: Typ.mode,
  self: Typ.self,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type info_typ = {
  cls: Term.UTyp.cls,
  ty: Typ.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type info =
  | Invalid
  | InfoExp(info_exp)
  | InfoPat(info_pat)
  | InfoTyp(info_typ);

type info_map = Id.Map.t(info);

[@deriving (show({with_path: false}), sexp, yojson)]
type error_status =
  | InHole
  | NotInHole
  | AtLeast(Typ.t);

//TODO(andrew): is this a correct implementation?
let union_uses = List.fold_left((uses1, uses2) => uses1 @ uses2, []);

let union_m =
  List.fold_left(
    (m1, m2) => Id.Map.union((_, _, b) => Some(b), m1, m2),
    Id.Map.empty,
  );

// What the type would be as if after hole-fixing
let ty_of_self: Typ.self => Typ.t =
  fun
  | Free => Unknown
  | Just(t) => t
  | Joined(tys) =>
    switch (Typ.join_all(tys)) {
    | None => Unknown
    | Some(t) => t
    };

let error_status = (info: info): error_status =>
  switch (info) {
  | InfoExp({mode: Syn, self: _, _}) => AtLeast(Unknown)
  | InfoExp({mode: Ana(_), self: Free, _}) => InHole
  | InfoExp({mode: Ana(ty_ana), self: Just(ty_syn), _}) =>
    switch (Typ.join(ty_ana, ty_syn)) {
    | None => InHole
    | Some(ty) => AtLeast(ty)
    }
  | InfoExp({mode: Ana(ty_ana), self: Joined(ty_syn), _}) =>
    switch (Typ.join_all([ty_ana] @ ty_syn)) {
    | None => InHole
    | Some(ty) => AtLeast(ty)
    }
  | InfoTyp(_) => NotInHole
  | InfoPat(_) => NotInHole //TODO
  | Invalid => InHole
  };

let remove_ctx_from_uses = (ctx: Typ.ctx, uses: Typ.co_ctx): Typ.co_ctx =>
  VarMap.filter(
    ((k, _)) =>
      switch (VarMap.lookup(ctx, k)) {
      | None => true
      | Some(_) => false
      },
    uses,
  );

let rec uexp_to_info_map =
        (
          ~m=Id.Map.empty,
          ~ctx=VarMap.empty,
          ~mode=Typ.Syn,
          {id, term}: Term.UExp.t,
        )
        : (Typ.t, Typ.co_ctx, info_map) => {
  let cls = Term.UExp.cls_of_term(term);
  let go = uexp_to_info_map(~ctx);
  let addm = i => Id.Map.add(id, i, m);
  let add = (~self, ~uses, m) => (
    ty_of_self(self),
    uses,
    Id.Map.add(id, InfoExp({cls, self, mode, ctx, uses}), m),
  );
  let atomic = self => (
    ty_of_self(self),
    [],
    addm(InfoExp({cls, self, mode, ctx, uses: []})),
  );
  let binop = (e1, e2, ty1, ty2, ty_out) => {
    let (_, uses1, m1) = go(~mode=ty1, e1);
    let (_, uses2, m2) = go(~mode=ty2, e2);
    add(~self=ty_out, ~uses=union_uses([uses1, uses2]), union_m([m1, m2]));
  };
  switch (term) {
  | Invalid(_p) => (Unknown, [], addm(Invalid))
  | EmptyHole => atomic(Free)
  | Bool(_) => atomic(Just(Bool))
  | Int(_) => atomic(Just(Int))
  | Var(name) =>
    switch (VarMap.lookup(ctx, name)) {
    | None => atomic(Free)
    | Some(ce) =>
      add(~self=Just(ce.typ), ~uses=[(name, [{id, mode}])], m)
    }
  | OpInt(Plus, e1, e2) => binop(e1, e2, Ana(Int), Ana(Int), Just(Int))
  | OpInt(Lt, e1, e2) => binop(e1, e2, Ana(Int), Ana(Int), Just(Bool))
  | OpBool(And, e1, e2) => binop(e1, e2, Ana(Bool), Ana(Bool), Just(Bool))
  | Pair(e1, e2) =>
    let (mode_l: Typ.mode, mode_r: Typ.mode) =
      switch (mode) {
      | Syn => (Syn, Syn)
      | Ana(ty) =>
        let (ty_l, ty_r) = Typ.matched_prod(ty);
        (Ana(ty_l), Ana(ty_r));
      };
    let (ty1, uses1, m1) = go(~mode=mode_l, e1);
    let (ty2, uses2, m2) = go(~mode=mode_r, e2);
    add(
      ~self=Just(Prod(ty1, ty2)),
      ~uses=union_uses([uses1, uses2]),
      union_m([m1, m2]),
    );
  | If(cond, e1, e2) =>
    let (_, uses_e0, m1) = go(~mode=Ana(Bool), cond);
    let (ty_e1, uses_e1, m2) = go(~mode, e1);
    let (ty_e2, uses_e2, m3) = go(~mode, e2);
    add(
      ~self=Joined([ty_e1, ty_e2]),
      ~uses=union_uses([uses_e0, uses_e1, uses_e2]),
      union_m([m1, m2, m3]),
    );
  | Ap(fn, arg) =>
    // NOTE: funpos currently set to Ana instead of Syn
    let (ty_fn, uses_fn, m_fn) =
      uexp_to_info_map(~ctx, ~mode=Ana(Arrow(Unknown, Unknown)), fn);
    let (ty_in, ty_out) = Typ.matched_arrow(ty_fn);
    let (_, uses_arg, m_arg) =
      uexp_to_info_map(~ctx, ~mode=Ana(ty_in), arg);
    add(
      ~self=Just(ty_out),
      ~uses=union_uses([uses_fn, uses_arg]),
      union_m([m_fn, m_arg]),
    );
  | Fun(pat, body) =>
    let (mode_pat: Typ.mode, mode_body: Typ.mode) =
      switch (mode) {
      | Syn => (Syn, Syn)
      | Ana(ty) =>
        let (ty_in, ty_out) = Typ.matched_arrow(ty);
        (Ana(ty_in), Ana(ty_out));
      };
    let (ty_pat, ctx_pat, m_pat) = upat_to_info_map(~mode=mode_pat, pat);
    let ctx_body = VarMap.union(ctx, ctx_pat);
    let (ty_body, uses_body, m_body) =
      uexp_to_info_map(~ctx=ctx_body, ~mode=mode_body, body);
    add(
      ~self=Just(Arrow(ty_pat, ty_body)),
      ~uses=remove_ctx_from_uses(ctx_pat, uses_body),
      union_m([m_pat, m_body]),
    );
  | FunAnn(pat, typ, body) =>
    let (typ_typ, m_typ) = utyp_to_info_map(typ);
    let (mode_pat: Typ.mode, mode_body: Typ.mode) =
      switch (mode) {
      | Syn => (Syn, Syn)
      | Ana(ty) =>
        let (ty_in, ty_out) = Typ.matched_arrow(ty);
        let ty_in' =
          switch (Typ.join(ty_in, typ_typ)) {
          | None => ty_in
          | Some(ty) => ty
          };
        (Ana(ty_in'), Ana(ty_out));
      };
    let (ty_pat, ctx_pat, m_pat) = upat_to_info_map(~mode=mode_pat, pat);
    let ctx_body = VarMap.union(ctx_pat, ctx);
    let (ty_body, uses_body, m_body) =
      uexp_to_info_map(~ctx=ctx_body, ~mode=mode_body, body);
    add(
      ~self=Just(Arrow(ty_pat, ty_body)),
      ~uses=remove_ctx_from_uses(ctx_pat, uses_body),
      union_m([m_pat, m_body, m_typ]),
    );
  | Let(pat, def, body) =>
    let (ty_pat, _ctx_pat, m_pat) = upat_to_info_map(~mode=Syn, pat);
    let (ty_def, uses_def, m_def) =
      uexp_to_info_map(~ctx, ~mode=Ana(ty_pat), def);
    // ana pat to incorporate def type into ctx
    let (_, ctx_pat_ana, _) = upat_to_info_map(~mode=Ana(ty_def), pat);
    let ctx = VarMap.union(ctx, ctx_pat_ana);
    let (ty_body, uses_body, m_body) = uexp_to_info_map(~ctx, ~mode, body);
    add(
      ~self=Just(ty_body),
      ~uses=
        union_uses([uses_def, remove_ctx_from_uses(ctx_pat_ana, uses_body)]),
      union_m([m_pat, m_def, m_body]),
    );
  | LetAnn(pat, typ, def, body) =>
    let (typ_typ, m_typ) = utyp_to_info_map(typ);
    let (ty_pat, _ctx_pat, m_pat) =
      upat_to_info_map(~mode=Ana(typ_typ), pat);
    let (ty_def, uses_def, m_def) =
      uexp_to_info_map(~ctx, ~mode=Ana(ty_pat), def);
    // joint_ty if consistent, otherwise pat type wins
    let joint_ty =
      switch (Typ.join(ty_def, ty_pat)) {
      | None => ty_pat
      | Some(ty) => ty
      };
    // ana pat to incorporate def type into ctx
    let (_, ctx_pat_ana, _) = upat_to_info_map(~mode=Ana(joint_ty), pat);
    let ctx = VarMap.union(ctx, ctx_pat_ana);
    let (ty_body, uses_body, m_body) = uexp_to_info_map(~ctx, ~mode, body);
    add(
      ~self=Just(ty_body),
      ~uses=
        union_uses([uses_def, remove_ctx_from_uses(ctx_pat_ana, uses_body)]),
      union_m([m_pat, m_typ, m_def, m_body]),
    );
  };
}
and upat_to_info_map =
    (
      ~m=Id.Map.empty,
      //~ctx as _=VarMap.empty,
      ~mode: Typ.mode=Typ.Syn,
      {id, term}: Term.UPat.t,
    )
    : (Typ.t, Typ.ctx, info_map) => {
  let cls = Term.UPat.cls_of_term(term);
  let addih = (~self: Typ.self, m) => (
    ty_of_self(self),
    [],
    Id.Map.add(id, InfoPat({cls, mode, self}), m),
  );
  let add = i => addih(~self=i, m);
  switch (term) {
  | Invalid(_p) => add(Free)
  | EmptyHole => add(Free)
  | Wild => add(Just(Unknown))
  | Int(_) => add(Just(Int))
  | Bool(_) => add(Just(Bool))
  | Var(name) =>
    let typ =
      switch (mode) {
      | Syn => Typ.Unknown
      | Ana(ty) => ty
      };
    let ctx: Typ.ctx = [(name, {id, typ})];
    (
      typ,
      ctx,
      Id.Map.add(id, InfoPat({cls, mode, self: Free}), m) //TODO(andrew): Free doesnt make sense
    );
  | Pair(p1, p2) =>
    let (mode_l: Typ.mode, mode_r: Typ.mode) =
      switch (mode) {
      | Syn => (Syn, Syn)
      | Ana(ty) =>
        let (ty_l, ty_r) = Typ.matched_prod(ty);
        (Ana(ty_l), Ana(ty_r));
      };
    let (ty_p1, ctx_p1, m_p1) = upat_to_info_map(~mode=mode_l, p1);
    let (ty_p2, ctx_p2, m_p2) = upat_to_info_map(~mode=mode_r, p2);
    let ctx = VarMap.union(ctx_p1, ctx_p2);
    let ty = Typ.Prod(ty_p1, ty_p2);
    (
      ty,
      ctx,
      Id.Map.add(
        id,
        InfoPat({cls, mode, self: Just(ty)}),
        union_m([m, m_p1, m_p2]),
      ),
    );
  };
}
and utyp_to_info_map =
    (
      ~m=Id.Map.empty,
      ~ctx as _=VarMap.empty,
      ~mode as _=Typ.Syn,
      {id, term} as utyp: Term.UTyp.t,
    )
    : (Typ.t, info_map) => {
  let cls = Term.UTyp.cls_of_term(term);
  let addm = i => Id.Map.add(id, i, m);
  switch (term) {
  | Invalid(_)
  | EmptyHole
  | Int
  | Bool
  | Arrow(_)
  | Prod(_) =>
    let ty = Term.utyp_to_ty(utyp);
    (ty, addm(InfoTyp({cls, ty})));
  };
};

/*

 planning for a better let:

 HAZEL:
        let* ty_p = Statics_Pat.syn_moded(ctx, p);
        let def_ctx = extend_let_def_ctx(ctx, p, def);
        let* _ = ana(def_ctx, def, ty_p);
        let* ty_def = syn(def_ctx, def);
        Statics_Pat.ana(ctx, p, ty_def);

        here: want to make sure that the info in the pattern
        takes into account the type of the definition and vice versa.
        but don't want to have errors in the pattern due to the definition, only augmenting the type.
        we do want to have errors in the definition due to the pattern, as well as augmentation.
        idea:
        get the self types of both (call with mode=Syn?). this doesn't actually need a context atm.
        ignore other returned values from those calls.
        if they are consistent, call again on pat in NEW mode Augment(ty). this is same
        as Ana(ty) in that it propagates type information down but where we also know
        the thing can synethesize something consistent with ty.
        if they are not consistent, use pat info_map from original Syn call.
        for the def, just call Ana(pat_ty) on it and merge with the stuff from the pat.
        finally, call on the body with the new ctx are whatever the original mode was.

        */
