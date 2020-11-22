module.exports = grammar({
    name: 'erlang',

    word: $ => $._raw_atom,

    extras: $ => [
        /[\x00-\x20\x80-\xA0]/,
        $._comment
    ],

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
            $.atom,
            $.var,
            $.wildcard,
        ),

        wildcard: $ => '_',

        var: $ => /[_A-Z\xC0-\xD6\xD8-\xDE][_@a-zA-Z0-9\xC0-\xD6\xD8-\xDE\xDF-\xF6\xF8-\xFF]*/,

        atom: $ => choice($._raw_atom, $._quoted_atom),

        _raw_atom: $ => /[a-z\xDF-\xF6\xF8-\xFF][_@a-zA-Z0-9\xC0-\xD6\xD8-\xDE\xDF-\xF6\xF8-\xFF]*/,

        _quoted_atom: $ => seq("'", /[^'\\]*/, "'"),

        _comment: $ => /%[^\n]*/,
    }
});
