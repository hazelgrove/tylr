open Util;

module type S = {let d: Direction.t;};

module L = {
  let d = Direction.Left;
};
module R = {
  let d = Direction.Right;
};
