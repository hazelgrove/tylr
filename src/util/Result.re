include Base.Result;

let to_option = Stdlib.Result.to_option;

module Syntax = {
  let ( let* ) = (result, f) => bind(~f, result);
  let (let+) = (result, f) => map(~f, result);
  let (let/) = (r, f) =>
    switch (r) {
    | Ok(ok) => Ok(ok)
    | Error(err) => f(err)
    };
};
