[@deriving show]
type t = {
  id: Id.t,
  content: string,
};

let space = " ";
let linebreak = "⏎"; //"¶";//

let mk_space = id => {content: space, id};

let id = w => w.id;
