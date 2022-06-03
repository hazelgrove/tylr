type movement =
  | Char
  | Mono
  | Token;

type settings = {mutable movement};
let s = {movement: Char};
