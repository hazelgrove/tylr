module Syntax = {
  let (let/) = (c, f) => c == 0 ? f() : c;
};
