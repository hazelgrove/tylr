open Core;

type reason =
  | Unrecognized
  | Failure(Failure.t);

type t = {
  reason,
  prior_attempts: int,
};

let mk = (~prior_attempts=0, reason) => {reason, prior_attempts};

let replace_or_increment_attempts = (reason, failed_input: t) =>
  reason == failed_input.reason
    ? {...failed_input, prior_attempts: failed_input.prior_attempts + 1}
    : mk(reason);
