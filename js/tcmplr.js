////// The TCMPLR Template Compiler

var lib = module.exports;

///// Nodes

lib.make_node_type = function(name, supertype) {
    var type = Object.create(supertype);
    type.name = name;
    return type;
};

lib.NODE = lib.make_node_type("node", Object.prototype);

lib.make_node = function(type, id, fields) {
    var node = Object.create(type);
    node.id = id;
    node.type = type;
    node.fields = fields;
    return node;
};

///// Anchors

lib.Anchor = function Anchor(href, title, node) {
    this.href = href;
    this.title = title;
    this.node = node;
};

lib.make_anchor = function(href) {
    return new lib.Anchor(href, null, null);
};

lib.make_titled_anchor = function(href, title) {
    return new lib.Anchor(href, title, null);
};

lib.make_immediate_anchor = function(node) {
    return new lib.Anchor(null, null, node);
};

lib.Anchor.prototype.resolve_anchor = function(environment) {
    if (this.node) {
        return this.node;
    } else {
        // Horrible kludge: this should really use a Qua VM API
        var node = environment.bindings["variable:" + this.href];
        if (!node) {
            throw "node not found: " + this.href;
        }
        this.node = node;
        return node;
    }
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
    out("</" + this.tag_name + ">\n");
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
    return "rt.node_field(env,anchor,node,\"" + this.field_name + "\",\"" + this.template_name + "\")";
};

lib.AnchorFieldInstruction = function AnchorFieldInstruction(field_name, template_name) {
    this.field_name = field_name;
    this.template_name = template_name;
};

lib.AnchorFieldInstruction.prototype = new lib.Instruction();

lib.AnchorFieldInstruction.prototype.compile_instruction = function() {
    return "rt.anchor_field(env,anchor,node,\"" + this.field_name + "\",\"" + this.template_name + "\")";
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

lib.FUNCTION_DEFINITION = "(function(rt,env,anchor,node){";

lib.instructions_to_js_function = function(instructions) {
    return lib.FUNCTION_DEFINITION + "return " +
        (instructions.map(function(instruction) {
            return instruction.compile_instruction();
        }).join("+"))
        + "; })";
};

///// Runtime

lib.rt = Object.create(null);

lib.rt.node_field = function(env, anchor, node, field_name, template_name) {
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
                var node = field_value.resolve_anchor(env);
                if (!node.fields) {
                    throw "not a node: " + node;
                }
                var template = node[template_name];
                if (!template) {
                    throw "template not found: " + template_name;
                }
                str += template(lib.rt, env, anchor, node);
            } else {
                throw "not a field value: " + field_value;
            }
        }
        return str;
    } else {
        return "";
    }
};

lib.rt.anchor_field = function(env, anchor, node, field_name, template_name) {
    return "ANCHOR TBD";
};

///// Rendering

lib.render = function(env, node, template) {
    return template(lib.rt, env, lib.make_immediate_anchor(node), node);
};
