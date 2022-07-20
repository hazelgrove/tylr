open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type typ =
  | Unknown
  | Int
  | Bool
  | Arrow(typ, typ);

[@deriving (show({with_path: false}), sexp, yojson)]
type term_t =
  | InvalidTyp(Piece.t)
  | EmptyHoleTyp
  | Int
  | Bool
  | Arrow(utyp, utyp)
and utyp = {
  id: Id.t,
  term_t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type term_p =
  | InvalidPat(Piece.t)
  | EmptyHolePat
  | Wild
  | IntPat(int)
  | BoolPat(bool)
  | VarPat(Token.t)
and upat = {
  id: Id.t,
  term_p,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type exp_op_int =
  | Plus
  | Lt;

[@deriving (show({with_path: false}), sexp, yojson)]
type exp_op_bool =
  | And;

[@deriving (show({with_path: false}), sexp, yojson)]
type uexp = {
  id: Id.t,
  term,
}
and term =
  | InvalidExp(Piece.t) //everything? text? keyword?
  //| InvalidSegment(Segment.t)
  | EmptyHole
  //| Triv
  | Bool(bool)
  | Int(int)
  | Fun(upat, uexp)
  | FunAnn(upat, utyp, uexp)
  | Var(Token.t)
  | Let(upat, uexp, uexp)
  | LetAnn(upat, utyp, uexp, uexp)
  | Ap(uexp, uexp)
  //| ApBuiltin(Token.t, list(uexp))
  // maybe everything with fn semantics should be a builtin e.g. plus??
  | If(uexp, uexp, uexp)
  | OpInt(exp_op_int, uexp, uexp)
  | OpBool(exp_op_bool, uexp, uexp);

let piece_and_kids = (ps, skel: Skel.t): (Piece.t, list(Skel.t)) => {
  let at = List.nth(ps);
  switch (skel) {
  | Op(idx) => (at(idx), [])
  | Pre(idx, skel') => (at(idx), [skel'])
  | Post(skel', idx) => (at(idx), [skel'])
  | Bin(skel_l, idx, skel_r) => (at(idx), [skel_l, skel_r])
  };
};

let rec of_seg_and_skel = (ps: Segment.t, skel: Skel.t): uexp => {
  let (p, kids) = piece_and_kids(ps, skel);
  of_piece(p, List.map(of_seg_and_skel(ps), kids));
}
and of_piece = (p: Piece.t, children_h: list(uexp)): uexp => {
  let invalid = (p: Piece.t) => {id: (-1), term: InvalidExp(p)};
  switch (p) {
  | Whitespace(_) => invalid(p)
  | Grout({id, shape}) =>
    switch (shape) {
    | Convex => {id, term: EmptyHole}
    | Concave => invalid(p)
    }
  | Tile({id, label, children, mold: _, shards: _} as t) =>
    // TODO(andrew): do better than switching label
    let term =
      switch (/*mold.out,*/ label, children_h, children) {
      | _ when !Tile.is_complete(t) => InvalidExp(p)
      | ([t], [], []) when Form.is_bool(t) => Bool(bool_of_string(t))
      | ([t], [], []) when Form.is_int(t) => Int(int_of_string(t))
      | ([t], [], []) when Form.is_var(t) => Var(t)
      | (["+"], [l, r], []) => OpInt(Plus, l, r)
      | (["<"], [l, r], []) => OpInt(Lt, l, r)
      | (["&&"], [l, r], []) => OpBool(And, l, r)
      | (["fun", "->"], [body], [pat]) => Fun(upat_of_seg(pat), body)
      | (["fun", ":", "->"], [body], [pat, typ]) =>
        FunAnn(upat_of_seg(pat), utyp_of_seg(typ), body)
      | (["let", "=", "in"], [body], [pat, def]) =>
        Let(upat_of_seg(pat), of_seg(def), body)
      | (["let", ":", "=", "in"], [body], [pat, typ, def]) =>
        LetAnn(upat_of_seg(pat), utyp_of_seg(typ), of_seg(def), body)
      | (["if", "then", "else"], [alt], [cond, conseq]) =>
        If(of_seg(cond), of_seg(conseq), alt)
      | (["(", ")"], [fn], [arg]) => Ap(fn, of_seg(arg))
      //TODO(andrew): more cases
      | _ => InvalidExp(p)
      };
    {id, term};
  };
}
and of_seg = (ps: Segment.t): uexp =>
  ps |> Segment.skel |> of_seg_and_skel(ps)
and of_seg_and_skel_pat = (ps: Segment.t, skel: Skel.t): upat => {
  let (p, kids) = piece_and_kids(ps, skel);
  of_piece_pat(p, List.map(of_seg_and_skel_pat(ps), kids));
}
and upat_of_seg = (ps: Segment.t): upat =>
  ps |> Segment.skel |> of_seg_and_skel_pat(ps)
and of_piece_pat = (p: Piece.t, children_h: list(upat)): upat => {
  let invalid = {id: (-1), term_p: InvalidPat(p)};
  switch (p) {
  | Whitespace(_) => invalid
  | Grout({id, shape}) =>
    switch (shape) {
    | Convex => {id, term_p: EmptyHolePat}
    | Concave => invalid
    }
  | Tile({id, label, children, mold: _, shards: _} as t) =>
    // TODO(andrew): do better than switching label
    let term_p =
      switch (/*mold.out,*/ label, children_h, children) {
      | _ when !Tile.is_complete(t) => InvalidPat(p)
      | ([t], [], []) when Form.is_bool(t) => BoolPat(bool_of_string(t))
      | ([t], [], []) when Form.is_int(t) => IntPat(int_of_string(t))
      | ([t], [], []) when Form.is_var(t) => VarPat(t)
      | ([t], [], []) when Form.is_wild(t) => Wild
      | _ => InvalidPat(p)
      };
    {id, term_p};
  };
}
and of_seg_and_skel_typ = (ps: Segment.t, skel: Skel.t): utyp => {
  let (p, kids) = piece_and_kids(ps, skel);
  of_piece_typ(p, List.map(of_seg_and_skel_typ(ps), kids));
}
and utyp_of_seg = (ps: Segment.t): utyp =>
  ps |> Segment.skel |> of_seg_and_skel_typ(ps)
and of_piece_typ = (p: Piece.t, children_h: list(utyp)): utyp => {
  let invalid = {id: (-1), term_t: InvalidTyp(p)};
  switch (p) {
  | Whitespace(_) => invalid
  | Grout({id, shape}) =>
    switch (shape) {
    | Convex => {id, term_t: EmptyHoleTyp}
    | Concave => invalid
    }
  | Tile({id, label, children, mold: _, shards: _} as t) =>
    // TODO(andrew): do better than switching label
    let term_t =
      switch (/*mold.out,*/ label, children_h, children) {
      | _ when !Tile.is_complete(t) => InvalidTyp(p)
      | (["Int"], [], []) => Int
      | (["Bool"], [], []) => Bool
      | (["->"], [l, r], []) => Arrow(l, r)
      | _ => InvalidTyp(p)
      };
    {id, term_t};
  };
};

let zip_to_uexp = (z: Zipper.t): uexp => z |> Zipper.zip |> of_seg;

// TYPES:

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | Syn
  | Ana(utyp);

[@deriving (show({with_path: false}), sexp, yojson)]
type ctx_entry = {
  id: Id.t,
  typ: utyp,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type ctx = VarMap.t_(ctx_entry);

[@deriving (show({with_path: false}), sexp, yojson)]
type co_ctx_item = {
  id: Id.t,
  mode,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type co_ctx_entry = list(co_ctx_item);

[@deriving (show({with_path: false}), sexp, yojson)]
type co_ctx = VarMap.t_(co_ctx_entry);

[@deriving (show({with_path: false}), sexp, yojson)]
type actual =
  // is this just a list??
  | Just(utyp)
  | Joined(list(utyp))
  | Free;

[@deriving (show({with_path: false}), sexp, yojson)]
type info_exp = {
  actual,
  mode, //add funpos
  ctx,
  co_ctx,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type info =
  | InfoExp(info_exp)
  | InfoPat(info_exp); //TODO(andrew)

type info_map = Id.Map.t(info);

let uexp_to_info_map =
    (
      ~m=Id.Map.empty,
      ~ctx as _=VarMap.empty,
      ~mode as _=Syn,
      {id: _, term}: uexp,
    )
    : info_map => {
  //TODO(andrew): implement
  switch (term) {
  | InvalidExp(_p) => m
  | EmptyHole => m
  | Bool(_b) => m
  | Int(_i) => m
  | Fun(_upat, _uexp) => m
  | FunAnn(_upat, _utyp, _uexp) => m
  | Var(_name) => m
  | Let(_upat, _uexp, _uexp') => m
  | LetAnn(_upat, _utyp, _uexp, _uexp') => m
  | Ap(_uexp, _uexp') => m
  | If(_uexp, _uexp', _uexp'') => m
  | OpInt(_op_int, _uexp, _uexp') => m
  | OpBool(_op_bool, _uexp, _uexp') => m
  };
};

//let uexp_to_info_map = uexp_to_info_map'(Id.Map.empty);
