open Util;
open Cor;

type t = AltList.a_frame(Zipper.t, Action.t);

let empty = ([], []);

let zipper_before_restructuring = ((prefix, _): t): option(Zipper.t) => {
  let (_restructuring, before) =
    prefix
    |> ListUtil.take_while(
         fun
         | (_, (Subject.Restructuring(_), _)) => true
         | _ => false,
       );
  switch (before) {
  | [] => None
  | [(_, zipper), ..._] => Some(zipper)
  };
};
