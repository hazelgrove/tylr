include Base.Result;

let ok = ok => Ok(ok);
let err = err => Error(err);

let to_option = Stdlib.Result.to_option;

let get_exn = exn =>
  fun
  | Error(_) => raise(exn)
  | Ok(ok) => ok;
let get_fail = msg =>
  fun
  | Error(_) => failwith(msg)
  | Ok(ok) => ok;

module Syntax = {
  let ( let* ) = (result, f) => bind(~f, result);
  let (let+) = (result, f) => map(~f, result);
  let (let/) = (r, f) =>
    switch (r) {
    | Ok(ok) => Ok(ok)
    | Error(err) => f(err)
    };
};
