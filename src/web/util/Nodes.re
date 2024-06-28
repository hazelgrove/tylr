open Virtual_dom.Vdom;

let svg = Node.create_svg("svg");
let filter = Node.create_svg("filter");

let map_element = (f, node: Node.t): Node.t =>
  switch (node) {
  | Element(e) => Element(f(e))
  | _ => node
  };

let add_class = cls => map_element(Fun.flip(Node.Element.add_class, cls));
let add_classes = clss =>
  map_element(Fun.flip(Node.Element.add_classes, clss));

// let stop = attrs => Node.create_svg("stop", ~attrs, []);
