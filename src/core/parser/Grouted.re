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

// completes wald to terr in opposite orientation
let complete_wald = (baked: t, wald: Wald.t): Terr.t =>
  is_eq(baked)
  |> Options.get_exn(Invalid_argument("Grouted.complete_wald"))
  |> Chain.fold_right(
       (c, t, (cell, wald)) => (c, Wald.link(t, cell, wald)),
       c => (c, wald),
     )
  |> (((cell, wald)) => Terr.{cell, wald: Wald.rev(wald)});

let repad = (dst: Delim.t, baked: t, src: Delim.t, ~onto: Dir.t) =>
  baked
  |> Chain.map_link(Bound.node)
  |> Chain.consnoc(~hd=dst, ~ft=src)
  |> Chain.map_linked((d: Delim.t, (sw, cell), b: Delim.t) => {
       let (l, r) = Dir.order(onto, (b, d));
       let (_, l) = Delim.padding(l).h;
       let (r, _) = Delim.padding(r).h;
       let c = Walk.Swing.is_open(sw) ? Cell.repad(~l, cell, ~r) : cell;
       (sw, c);
     })
  |> Chain.unconsnoc_exn
  |> (((_, c, _)) => c)
  |> Chain.map_link(Bound.to_opt)
  |> Chain.map_link(Option.get);

// completes terr to meld in same orientation
let complete_terr = (baked: t, terr: Terr.t, ~onto, ~bound): Meld.t =>
  repad(bound, baked, Node(Terr.hd(terr)), ~onto)
  |> is_eq
  // is_eq(baked)
  |> Options.get_exn(Invalid_argument("Grouted.complete_terr"))
  |> Chain.fold_right(
       (cell, tok) => Meld.link(~cell, tok),
       cell => M(cell, terr.wald, terr.cell),
     );

let connect_eq = (dst: Token.t, baked: t, src: Terr.t, ~onto: Dir.t) =>
  repad(Node(dst), baked, Node(Terr.hd(src)), ~onto)
  |> Chain.Affix.cons(dst)
  |> Chain.Affix.fold_out(
       ~init=src,
       ~f=(tok, (swing, cell)) => {
         assert(Walk.Swing.height(swing) == 0);
         Terr.link(tok, cell);
       },
     );
let connect_neq = (dst: Token.t, baked: t, src: Bound.t(Terr.t), ~onto) =>
  repad(Node(dst), baked, Bound.map(Terr.hd, src), ~onto)
  |> Chain.Affix.cons(dst)
  |> Chain.Affix.fold_out(~init=Slope.empty, ~f=(tok, (swing, cell)) =>
       Walk.Swing.height(swing) == 0
         ? Slope.link(tok, cell) : Slope.cons(Terr.mk([tok], [cell]))
     );
