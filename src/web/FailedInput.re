open Core;

type reason =
  | Unrecognized
  | Failure(Failure.t);

type t = {
  reason,
  prior_attempts: int,
};

let mk = (~prior_attempts=0, reason) => {reason, prior_attempts};
