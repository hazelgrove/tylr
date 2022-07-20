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
