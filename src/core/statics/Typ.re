open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Unknown
  | Int
  | Bool
  | Arrow(t, t);

[@deriving (show({with_path: false}), sexp, yojson)]
type inherent =
  // is this just a list??
  | Just(t)
  | Joined(list(t))
  | Free;

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  //| FunPos
  | Syn
  | Ana(t);

[@deriving (show({with_path: false}), sexp, yojson)]
type ctx_entry = {
  id: Id.t,
  typ: t,
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

let rec join = (ty1: t, ty2: t): option(t) =>
  switch (ty1, ty2) {
  | (Unknown, _)
  | (_, Unknown) => Some(Unknown)
  | (Int, Int) => Some(Int)
  | (Bool, Bool) => Some(Bool)
  | (Arrow(ty1_1, ty1_2), Arrow(ty2_1, ty2_2)) =>
    switch (join(ty1_1, ty2_1), join(ty1_2, ty2_2)) {
    | (Some(ty1), Some(ty2)) => Some(Arrow(ty1, ty2))
    | _ => None
    }
  | _ => None
  };
