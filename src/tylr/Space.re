type shape =
  | Space
  | Newline;
type elem = {
  id: Id.t,
  shape,
};
type t = list(elem);

let empty = [];
let is_empty: t => bool = (==)(empty);

let mk_elem = shape => {
  let id = Id.Gen.next();
  {id, shape};
};

let split_cursor = (_: t) => failwith("todo split_cursor");
