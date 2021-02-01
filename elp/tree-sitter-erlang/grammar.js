/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
const PREC = {
  COMMENT: 1, // Prefer comments over regexes
  STRING: 2,  // In a string, prefer string characters over comments
};

module.exports = grammar({
    name: 'erlang_elp',

    word: $ => $._raw_atom,

    extras: $ => [
        /[\x00-\x20\x80-\xA0]/,
        $._comment
    ],

    supertypes: $ => [
        $._form,
        $._expr,
    ],

    rules: {
        source_file: $ => field('forms', repeat($._form)),

        _form: $ => choice(
            $.module_attribute,
            $.attribute,
            $.function
        ),

        module_attribute: $ => seq('-', 'module', '(', field('name', $.atom), ')', '.'),

        attribute: $ => seq('-', field('name', $.atom), '(', field('value', $._expr), ')', '.'),

        function: $ => seq(sepBy1(field('clauses', $.function_clause), ';'), '.'),

        function_clause: $ => seq(field('name', $.atom), field('arg_list', $.arg_list), '->', field('body', $.block)),

        // TODO: separate expr and patterns
        arg_list: $ => seq('(', sepBy(field('args', $._expr), ','), ')'),

        block: $ => sepBy1(field('exprs', $._expr), ','),

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

        // Precedence usage copied from
        // https://github.com/tree-sitter/tree-sitter-javascript/blob/master/grammar.js
        _quoted_atom: $ => seq(
            "'",
            repeat1(choice(token.immediate(prec(PREC.STRING, /[^'\n\\]+/)), $._escape)),
            token.immediate("'")),

        _escape: $ => token.immediate(seq(
            '\\',
            choice(
                /[0-7]{1,3}/,
                /x[0-9a-fA-F]{2}/,
                /x{[0-9a-fA-F]+}/,
                '\n',
                /[nrtvbfesd]/
            )
        )),

        _comment: $ => token(prec(PREC.COMMENT,/%[^\n]*/)),
    }
});

function sepBy1(rule, sep) {
    return seq(rule, repeat(seq(sep, rule)));
}

function sepBy(rule, sep) {
    return optional(sepBy1(rule, sep));
}
