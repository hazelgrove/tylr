// eventually this will have an id,
// preemptively making a record
[@deriving show]
type t = {
  id: Id.t,
  content: string,
};

let linebreak = "⏎";
