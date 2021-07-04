module Bar = {
  let state = ref(false);

  let mk = () => {
    let cls = state^ ? "just-failed-1" : "just-failed-0";
    state := ! state^;
    cls;
  };
};

module Pos = {
  let state = ref(false);

  let mk = () => {
    let cls = state^ ? "just-failed-1" : "just-failed-0";
    state := ! state^;
    cls;
  };
};
