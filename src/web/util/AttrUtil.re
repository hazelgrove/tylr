open Virtual_dom.Vdom.Attr;

let fstr = f => Printf.sprintf("%f", f);

let cx = f => create("cx", fstr(f));
let cy = f => create("cy", fstr(f));
let rx = f => create("rx", fstr(f));
let ry = f => create("ry", fstr(f));

let stroke_width = f => create("stroke-width", fstr(f));
let vector_effect = s => create("vector-effect", s);
let filter = s => create("filter", s);
