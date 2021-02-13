let flatten_tile: Tile_typ.t => AltList.t(Tessera.t, Term_typ.t) =
  T.get(
    fun
    | OpHole => A(OpHole, None)
    | Num => A(Num, None)
    | Bool => A(Bool, None)
    | Paren(body) => A(Paren_l, Some(B(body, A(Paren_r, None)))),
    () => raise(Term_typ.Void_pre),
    () => raise(Term_typ.Void_post),
    fun
    | BinHole => A(BinHole, None)
    | Arrow => A(Arrow, None),
  );

let unsort_s = failwith("todo");
let unsort = failwith("todo");

let rec sort_s = (ts: Unsorted.Tile.s): option(Tile_typ.s) =>
  ts |> List.map(sort) |> OptUtil.sequence
and sort = (tile: Unsorted.Tile.t): option(Tile_typ.t) =>
  tile
  |> Tile.get(
       fun
       | OpHole => Some(Tile.Op(OpHole))
       | Text(s) =>
         switch (s) {
         | "Num" => Some(Op(Num))
         | "Bool" => Some(Op(Bool))
         | _ => None
         }
       | Paren(body) => {
           let+ body = sort_s(body);
           Tile.Op(Paren(body));
         },
       fun
       | Lam(_)
       | Let(_) => None,
       fun
       | Ann(_)
       | Ap(_) => None,
       fun
       | Plus => None
       | Arrow => Some(Tile.Bin(Arrow))
       | BinHole => Some(Bin(BinHole)),
     );
