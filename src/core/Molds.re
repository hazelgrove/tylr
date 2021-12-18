open Mold;

type t = list(Mold.t);

let none = [];
let any = failwith("todo");

let pat = List.map(tips => [{tips, sorts: Sorts.pat}]);
let exp = List.map(tips => [{tips, sorts: Sorts.exp}]);

let any_sort = tips =>
  [Sorts.pat, Sorts.exp]
  |> List.map(sorts => {tips, sorts});

// let of_tile: Tile.t => t =
//   Tile.get(
//     fun
//     | ("", _) =>
//       // assuming all empty or all non-empty shards for now
//       any
//     | (s, []) when StringUtil.is_var(s) || StringUtil.is_num(s) =>
//       any_sort(Tips.op)
//     | ("(", [(_, ")")])
//     | ("λ", [(_, "{"), (_, "}")]) => any_sort(Tips.op)
//     | ("let", [(_, "="), (_, "in")]) => exp([Tips.pre])
//     | ("!", [])
//     | ("[", [(_, "]")]) => exp([Tips.post])
//     | ("+" | "-" | "*" | "/", [])
//     | ("?", (_, ":")) => exp([Tips.bin])
//     | _ => none
//   );

let of_token: Token.t => t =
  fun
  | "" => any
  | "(" => any_sort([Tips.pre]) @ [{tips: Tips.bin, sorts: Sorts.exp}]
  | ")" => any_sort([Tips.post])
  | "λ" | "let" => [{tips: Tips.pre, sorts: {l: Exp, r: Pat}}]
  | "." | "=" => [{tips: Tips.bin, sorts: {l: Pat, r: Exp}}]
  | "in" | "+" | "-" | "*" | "/" | "?" | ":" => exp([Tips.bin])
  | "!" => exp([Tips.post])
  | _ => none;

let of_shard = Shard.get(of_token);

let of_tile =
  Tile.get(tokens => {
    let mold_l = of_token(Aba.hd(tokens));
    let mold_r = of_token(Aba.last(tokens));
    Mold.merge(mold_l, mold_r);
  });
