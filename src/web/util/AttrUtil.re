open Virtual_dom.Vdom.Attr;

let cx = f => create("cx", Printf.sprintf("%f", f));
let cy = f => create("cy", Printf.sprintf("%f", f));
let rx = f => create("rx", Printf.sprintf("%f", f));
let ry = f => create("ry", Printf.sprintf("%f", f));
