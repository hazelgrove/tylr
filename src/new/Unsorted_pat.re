let flatten_tile: Tile_pat.t => AltList.t(Tessera.t, Term_pat.t) =
  Tile.get(
    fun
    | OpHole => A(OpHole, None)
    | Var(x) => A(Var(x), None)
    | Paren(body) => A(Paren_l, Some(B(body, A(Paren_r, None)))),
    () => raise(Term.Void_pre),
    fun
    | Ann(ann) => A(Ann(Typ.(to_unsorted_s(flatten_term(ann))))),
    fun
    | BinHole => A(BinHole, None),
  );

let unsort_s = failwith("todo");
let unsort = failwith("todo");

let rec sort_s = (ts: Unsorted.Tile.s): option(s) =>
  ts |> List.map(sort) |> OptUtil.sequence
and sort = (tile: Unsorted.Tile.t): option(Tile_typ.t) =>
  tile
  |> Tile.get(
       fun
       | OpHole => Some(Tile.Op(OpHole))
       | Text(s) =>
         if (StringUtil.is_var(s)) {
           Some(Op(Var(s)));
         } else {
           None;
         }
       | Paren(body) => {
           let+ body = sort_s(body);
           Tile.Op(Paren(body));
         },
       fun
       | Lam(_)
       | Let(_) => None,
       fun
       | Ap(_) => None
       | Ann(ann) => {
           let+ ann = Tile_typ.mk_term(Unsorted_typ.sort_s(ann));
           Tile.Post(Ann(ann));
         },
       fun
       | Arrow
       | Plus => None
       | BinHole => Some(Tile.Bin(BinHole)),
     );
