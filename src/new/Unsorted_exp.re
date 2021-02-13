let flatten_tile: Tile_exp.t => AltList.t(Tessera.t, Term_exp.t) =
  Tile.get(
    fun
    | OpHole => A(OpHole, None)
    | Num(n) => A(Num(n), None)
    | Var(x) => A(Var(x), None)
    | Paren(body) => A(Paren_l, Some(B(body, A(Paren_r, None)))),
    fun
    | Lam(p) =>
      A(Lam(Unsorted_pat.unsort_s(Tile_pat.flatten_term(p))), None)
    | Let(p, def) =>
      A(
        Let_eq(Unsorted_pat.unsort_s(Tile_pat.flatten_term(p))),
        Some(B(def, A(Let_in, None))),
      ),
    fun
    | Ap(_) => failwith("todo"),
    fun
    | Plus => A(Plus, None)
    | BinHole => A(BinHole, None),
  );

let unsort_s = failwith("todo");
let unsort = failwith("todo");

let rec sort_s = (ts: Unsorted.Tile.s): option(s) =>
  ts |> List.map(sort) |> OptUtil.sequence
and sort = (tile: Unsorted.Tile.t): option(Tile_exp.t) =>
  tile
  |> Tile.get(
       fun
       | OpHole => Some(Tile.Op(OpHole))
       | Text(s) =>
         if (StringUtil.is_num(s)) {
           Some(Op(Num(int_of_string(s))));
         } else if (StringUtil.is_var(s)) {
           Some(Op(Var(s)));
         } else {
           None;
         }
       | Paren(body) => {
           let+ body = sort_s(body);
           Tile.Op(Paren(body));
         },
       fun
       | Lam(p) => {
           let+ p = Unsorted_pat.sort_s(p);
           Tile.Pre(Lam(p));
         }
       | Let(p, def) => {
           let+ p = Unsorted_pat.sort_s(p)
           and+ def = sort_s(def);
           Tile.Pre(Let(p, def));
         },
       fun
       | Ann(_) => None
       | Ap(arg) => {
           let+ arg = sort_s(arg);
           Tile.Post(Ap(arg));
         },
       fun
       | Arrow => None
       | Plus => Some(Tile.Bin(Plus))
       | BinHole => Some(Bin(BinHole)),
     );
