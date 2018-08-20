////// The TCMPLR Template Compiler

var lib = module.exports;

///// Nodes

lib.make_node_type = function(name, supertype) {
    var type = Object.create(supertype);
    type.name = name;
    return type;
};

lib.NODE = lib.make_node_type("node", Object.prototype);

lib.make_node = function(type, reference, fields) {
    var node = Object.create(type);
    node.reference = reference;
    node.type = type;
    node.fields = fields;
    node.fragments = Object.create(null);
    return node;
};

///// Anchors

lib.Anchor = function Anchor(reference, title, node) {
    this.reference = reference;
    this.title = title;
    this.node = node;
};

lib.make_anchor = function(reference) {
    return new lib.Anchor(reference, null, null);
};

lib.make_titled_anchor = function(reference, title) {
    return new lib.Anchor(reference, title, null);
};

lib.make_immediate_anchor = function(node) {
    return new lib.Anchor(null, null, node);
};

lib.Anchor.prototype.resolve_anchor = function(store) {
    if (this.node) {
        return this.node;
    } else {
        var node = store.nodes[this.reference.to_html_url()];
        if (!node) {
            throw "node not found: " + this.reference.to_html_url();
        }
        this.node = node;
        return node;
    }
};

///// Store

lib.Store = function Store() {
    this.nodes = Object.create(null);
};

lib.Store.prototype.put_node = function(reference, node) {
    this.nodes[reference.to_html_url()] = node;
};

lib.Store.prototype.get_node = function(reference) {
    return this.nodes[reference.to_html_url()];
};

///// References

lib.Reference = function Reference(path, fragment) {
    this.path = path;
    this.fragment = fragment;
};

lib.Reference.prototype.to_html_url = function() {
    return this.path.join("/") + ".html" + (this.fragment ? ("?" + this.fragment) : "");
};

///// Templates

lib.Template = function Template() {};

//// Tag Templates

lib.TagTemplate = function(tag_name, attributes, child_templates) {
    this.tag_name = tag_name;
    this.attributes = attributes;
    this.child_templates = child_templates;
};

lib.TagTemplate.prototype = new lib.Template();

lib.compile_template = function(template, out) {
    if (typeof(template) === "string") {
        out(template);
    } else if (template instanceof lib.Template) {
        template.compile_template(out);
    } else {
        throw "not a template: " + template;
    }
}

lib.TagTemplate.prototype.compile_template = function(out) {
    out("<" + this.tag_name);
    this.compile_attributes(out);
    out(">");
    this.compile_children(out);
    out("</" + this.tag_name + ">");
};

lib.TagTemplate.prototype.compile_attributes = function(out) {
    for (var attribute_name in this.attributes) {
        out(" " + attribute_name + "=\"");
        lib.compile_template(this.attributes[attribute_name], out);
        out("\"");
    }
};

lib.TagTemplate.prototype.compile_children = function(out) {
    for (var child_index in this.child_templates) {
        lib.compile_template(this.child_templates[child_index], out);
    }
};

//// Field Templates

lib.NodeFieldTemplate = function NodeFieldTemplate(field_name, template_name) {
    this.field_name = field_name;
    this.template_name = template_name;
};

lib.NodeFieldTemplate.prototype = new lib.Template();

lib.NodeFieldTemplate.prototype.compile_template = function(out) {
    out(new lib.NodeFieldInstruction(this.field_name, this.template_name));
};

lib.AnchorFieldTemplate = function AnchorFieldTemplate(field_name, template_name) {
    this.field_name = field_name;
    this.template_name = template_name;
};

lib.AnchorFieldTemplate.prototype = new lib.Template();

lib.AnchorFieldTemplate.prototype.compile_template = function(out) {
    out(new lib.AnchorFieldInstruction(this.field_name, this.template_name));
};

//// Node Link Template

lib.NodeLinkTemplate = function NodeLinkTemplate() {};
lib.NodeLinkTemplate.prototype = new lib.Template();
lib.NodeLinkTemplate.prototype.compile_template = function(out) {
    out(new lib.NodeLinkInstruction(this.field_name, this.template_name));
};

///// Instructions

lib.Instruction = function Instruction() {};

//// String Instructions

lib.StringInstruction = function StringInstruction(string) {
    this.string = string;
};

lib.StringInstruction.prototype = new lib.Instruction();

lib.StringInstruction.prototype.compile_instruction = function() {
    return JSON.stringify(this.string);
};

//// Field Instructions

lib.NodeFieldInstruction = function NodeFieldInstruction(field_name, template_name) {
    this.field_name = field_name;
    this.template_name = template_name;
};

lib.NodeFieldInstruction.prototype = new lib.Instruction();

lib.NodeFieldInstruction.prototype.compile_instruction = function() {
    return "rt.node_field(store,anchor,node,\"" + this.field_name + "\",\"" + this.template_name + "\")";
};

lib.AnchorFieldInstruction = function AnchorFieldInstruction(field_name, template_name) {
    this.field_name = field_name;
    this.template_name = template_name;
};

lib.AnchorFieldInstruction.prototype = new lib.Instruction();

lib.AnchorFieldInstruction.prototype.compile_instruction = function() {
    return "rt.anchor_field(store,anchor,node,\"" + this.field_name + "\",\"" + this.template_name + "\")";
};

//// Node Link Instruction

lib.NodeLinkInstruction = function NodeLinkInstruction() {};
lib.NodeLinkInstruction.prototype = new lib.Instruction();
lib.NodeLinkInstruction.prototype.compile_instruction = function() {
    return "rt.node_link(node)";
};

///// Compilation Process

lib.compile = function(template) {
    var instructions = lib.template_to_instructions(template);
    var js_code = lib.instructions_to_js_function(instructions);
    return eval(js_code);
};

lib.template_to_instructions = function(template) {
    var instructions = [];
    var str = null; // used to fuse adjacent strings
    function out(obj) {
        if (typeof(obj) === "string") {
            if (typeof(str) === "string") {
                str += obj;
            } else {
                str = obj;
            }
        } else {
            if (typeof(str) === "string") {
                instructions.push(new lib.StringInstruction(str));
                str = null;
            }
            if (obj instanceof lib.Instruction) {
                instructions.push(obj);
            } else {
                throw "not an instruction: " + obj;
            }
        }
    }
    lib.compile_template(template, out);
    if (typeof(str) === "string") {
        instructions.push(new lib.StringInstruction(str));
    }
    return instructions;
}

lib.FUNCTION_DEFINITION = "(function(rt,store,anchor,node){";

lib.instructions_to_js_function = function(instructions) {
    return lib.FUNCTION_DEFINITION + "return " +
        (instructions.map(function(instruction) {
            return instruction.compile_instruction();
        }).join("+"))
        + "; })";
};

///// Runtime

lib.rt = Object.create(null);

lib.rt.node_field = function(store, anchor, node, field_name, template_name) {
    if (!node.fields) {
        throw "probably not a node: " + node;
    }
    var field_values = node.fields[field_name];
    if (field_values) {
        var str = "";
        for (var i in field_values) {
            var field_value = field_values[i];
            if (typeof(field_value) === "string") {
                str += field_value;
            } else if (field_value instanceof lib.Anchor) {
                var node = field_value.resolve_anchor(store);
                if (!node.fields) {
                    throw "not a node: " + node;
                }
                var template = node[template_name];
                if (!template) {
                    throw "template not found: " + template_name;
                }
                str += template(lib.rt, store, anchor, node);
            } else {
                throw "not a field value: " + field_value;
            }
        }
        return str;
    } else {
        return "";
    }
};

lib.rt.anchor_field = function(store, anchor, node, field_name, template_name) {
    return "ANCHOR TBD";
};

lib.rt.node_link = function(node) {
    return node.reference.to_html_url();
};

///// Rendering

lib.render = function(store, node, template) {
    return template(lib.rt, store, lib.make_immediate_anchor(node), node);
};
