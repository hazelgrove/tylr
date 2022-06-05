//TODO(andrew): autoformatter

[@deriving show]
type padding =
  | None
  | Bi
  | Pre
  | Post;

let padding: string => padding =
  fun
  | "fun"
  | "let" => Post
  | "=>"
  | "+"
  | "-"
  | "*"
  | "/"
  | ","
  | "="
  | "in"
  | "?"
  | ":" => Bi
  | "("
  | ")"
  | "["
  | "]"
  | "}"
  | _ => None;

let format: Zipper.t => Zipper.t = z => z;
