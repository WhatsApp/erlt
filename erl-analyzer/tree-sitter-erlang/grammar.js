module.exports = grammar({
    name: 'erlang',

    rules: {
        source_file: $ => repeat($._form),

        _form: $ => choice(
            $.module_attribute,
            $.attribute,
            // $.function
        ),

        module_attribute: $ => seq('-', 'module', '(', $.atom, ')', '.'),

        attribute: $ => seq('-', $.atom, '(', $._expr, ')', '.'),

        _expr: $ => choice(
            $.atom
        ),

        atom: $ => /[a-z][a-zA-Z0-9_]*/,
    }
});
