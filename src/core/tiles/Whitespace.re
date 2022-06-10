[@deriving show]
type t = {
  id: Id.t,
  content: string,
};

let id = w => w.id;

let linebreak = "⏎";
let space = " ";
