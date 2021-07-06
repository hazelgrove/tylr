open Sexplib.Std;
open Core;

[@deriving sexp]
type reason =
  | Unrecognized
  | Failure(Failure.t);

[@deriving sexp]
type t = {
  reason,
  prior_attempts: int,
};

let mk = (~prior_attempts=0, reason) => {reason, prior_attempts};

let replace_or_increment_attempts = (reason, failed_input: t) =>
  reason == failed_input.reason
    ? {...failed_input, prior_attempts: failed_input.prior_attempts + 1}
    : mk(reason);
