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
type info_typ = unit; //TODO(andrew)

[@deriving (show({with_path: false}), sexp, yojson)]
type info =
  | Invalid
  | InfoExp(info_exp)
  | InfoPat(info_pat)
  | InfoTyp(info_typ);

type info_map = Id.Map.t(info);

let uexp_to_info_map =
    (
      ~m=Id.Map.empty,
      ~ctx as _=VarMap.empty,
      ~mode as _=Typ.Syn,
      {id: _, term}: Term.uexp,
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
