open Stds;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Chain.t((Walk.Swing.t, Cell.t), Token.t);

let fold =
    (
      f_c: (_, 'acc) => 'acc,
      f_tc: (Token.t, _, 'acc) => 'acc,
      init: 'acc,
      baked: t,
    ) => {
  let ((ts, cs), c) = Chain.unsnoc(baked);
  let acc = List.fold_right2(f_tc, ts, cs, init);
  f_c(c, acc);
};

let is_eq = ((cells, toks): t) =>
  cells
  |> List.map(((swing, cell)) =>
       Walk.Swing.height(swing) == 0 ? Some(cell) : None
     )
  |> Options.for_all
  |> Option.map(cells => (cells, toks));

let to_walk: t => Walk.t = Chain.map(fst, (tok: Token.t) => tok.mtrl);

let is_neq = (grouted: t) => Walk.is_neq(to_walk(grouted));

// completes wald to terr in opposite orientation
let complete_wald = (baked: t, wald: Wald.t): Terr.t =>
  is_eq(baked)
  |> Options.get_exn(Invalid_argument("Grouted.complete_wald"))
  |> Chain.fold_right(
       (c, t, (cell, wald)) => (c, Wald.link(t, cell, wald)),
       c => (c, wald),
     )
  |> (((cell, wald)) => Terr.{cell, wald: Wald.rev(wald)});

// completes terr to meld in same orientation
let complete_terr = (baked: t, terr: Terr.t): Meld.t =>
  is_eq(baked)
  |> Options.get_exn(Invalid_argument("Grouted.complete_terr"))
  |> Chain.fold_right(
       (cell, tok) => Meld.link(~cell, tok),
       cell => M(cell, terr.wald, terr.cell),
     );
