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

let either =
  fun
  | Error(x)
  | Ok(x) => x;

module Syntax = {
  let ( let* ) = (result, f) => bind(~f, result);
  let (let+) = (result, f) => map(~f, result);
  let (let/) = (r, f) =>
    switch (r) {
    | Ok(ok) => Ok(ok)
    | Error(err) => f(err)
    };
};

module Thunk = {
  type t('ok, 'err) = unit => Base.Result.t('ok, 'err);
  let ok = (ok, ()) => Ok(ok);
  let err = (err, ()) => Error(err);
  let to_option = (r, ()) => to_option(r());
};
