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
            $.function
        ),

        module_attribute: $ => seq('-', 'module', '(', $.atom, ')', '.'),

        attribute: $ => seq('-', $.atom, '(', $._expr, ')', '.'),

        function: $ => seq(sepBy1($.function_clause, ';'), '.'),

        function_clause: $ => seq($.atom, $.arg_list, '->', $.block),

        // TODO: separate expr and patterns
        arg_list: $ => seq('(', sepBy($._expr, ','), ')'),

        block: $ => sepBy1($._expr, ','),

        _expr: $ => choice(
            $.atom,
            $.var,
            $.wildcard,
            $.string,
            $.char,
            $.integer,
            $.float,
        ),

        wildcard: $ => '_',

        var: $ => /[_A-Z\xC0-\xD6\xD8-\xDE][_@a-zA-Z0-9\xC0-\xD6\xD8-\xDE\xDF-\xF6\xF8-\xFF]*/,

        integer: $ => choice(
            /\d{1,2}#[0-9a-zA-Z](_?[0-9a-zA-Z])*/,
            /\d(_?\d)*/,
        ),

        float: $ => /\d(_?\d)*\.\d(_?\d)*([eE][+-]?\d(_?\d)*)?/,

        string: $ => seq('"', repeat(choice(/[^"\\]+/, $._escape)), '"'),

        char: $ => seq('$', choice(/[^\\]/, $._escape)),

        atom: $ => choice($._raw_atom, $._quoted_atom),

        _raw_atom: $ => /[a-z\xDF-\xF6\xF8-\xFF][_@a-zA-Z0-9\xC0-\xD6\xD8-\xDE\xDF-\xF6\xF8-\xFF]*/,

        _quoted_atom: $ => seq("'", repeat(choice(/[^'\\]+/, $._escape)), "'"),

        _escape: $ => token(seq('\\', choice(/[0-7]{1,3}/, /x[0-9a-fA-F]{2}/, /x{[0-9a-fA-F]+}/, '\n', /[nrtvbfesd]/))),

        _comment: $ => /%[^\n]*/,
    }
});

function sepBy1(rule, sep) {
    return seq(rule, repeat(seq(sep, rule)));
}

function sepBy(rule, sep) {
    return optional(sepBy1(rule, sep));
}
