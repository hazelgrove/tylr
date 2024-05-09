open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

[@deriving (show({with_path: false}), sexp, yojson, hash)]
type t = int;
let eq = Int.equal;
let compare = Int.compare;
let invalid = (-1);

module Map = Ptmap;

module Gen = {
  let t: ref(t) = ref(0);

  let next = (): t => {
    let id = t^;
    t := id + 1;
    id;
  };

  let value =
    fun
    | Some(id) => id
    | None => next();
};
