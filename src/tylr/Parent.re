type t = (Chain.t, Chain.t);

// let root = failwith("todo parent root");

[@warning "-27"]
let cmp_merge_l =
    (par: t, ~kid=?, c: Chain.t): option(Cmp.Result.t(Chain.t, t, t)) =>
  failwith("todo");

[@warning "-27"]
let cmp_merge_r =
    (c: Chain.t, ~kid=?, par: t): option(Cmp.Result.t(t, t, Chain.t)) =>
  failwith("todo");

[@warning "-27"]
let mold = (~match, ~kid=?, t, par) => failwith("todo mold");
