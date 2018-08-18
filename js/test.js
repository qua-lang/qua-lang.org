var lib = require("./tmplcmpl.js");
var nt1 = lib.make_node_type(lib.NODE);
var env = {};
var node1 = lib.make_node(nt1, "node-1", { "title": ["Hello world!"] });
env.node1 = node1;
var tmpl1 = new lib.TagTemplate("h1", {}, [new lib.TagTemplate("p",{},[]), new lib.NodeFieldTemplate("title", "moo")]);
var fn = lib.compile(tmpl1);
console.log(fn(lib.rt, env, lib.make_immediate_anchor(node1), node1));
