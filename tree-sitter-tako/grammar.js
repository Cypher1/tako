module.exports = grammar({
  name: 'tako',

  extras: ($) => [$.nesting_comment, $.single_line_comment, "\r", "\n", "\t", " "],
  rules: {
    // TODO: add the actual grammar rules
    source_file: ($) => seq(optional($.shebang), repeat($._statement)),
    _block: ($) => seq('{', $._block, '}'),
    _sequence: ($) => repeat(seq(optional($._statement), ';')),
    _statement: ($) => choice(
      $.definition,
      $._block,
      $.expression
    ),
    definition: ($) => seq($.ident, '=', $.expression),
    expression: ($) => $._inner_expression,
    _inner_expression: ($) => choice(
      seq('(', $.expression ,')'),
      $.string,
      $.number,
      $.color,
      $.ident
    ),
    number: ($) => choice(
      $.int_literal,
      $.float_literal
    ),
    comment: ($) => choice($.single_line_comment, $.nesting_comment),
    _nesting_comment_contents: ($) => choice(
      $.nesting_comment,
      $._anything,
    ),
    nesting_comment: ($) => seq(
      '/*',
      repeat($._nesting_comment_contents),
      '*/'
    ),
    _anything: (_) => choice('\*', '/', /[^*/]/),
    break: (_) => "break",
    continue: (_) => "continue",
    return: (_) => "return",
    forall: (_) => "forall",
    exists: (_) => "exists",
    given: (_) => "given",
    color: (_) => /#[a-fA-F0-9_]+/,
    ident: (_) => /[a-zA-Z][a-zA-Z0-9_]*/,
    string: $ => seq(
      '"',
      repeat(choice(
        $.escape_sequence,
        /[^\"]/,
      )),
      token.immediate('"'),
    ),
    escape_sequence: _ => token.immediate(
      seq('\\',
        choice(
          /[^xu]/,
          /u[0-9a-fA-F]{4}/,
          /u\{[0-9a-fA-F]+\}/,
          /x[0-9a-fA-F]{2}/,
        ),
      )),
    int_literal: (_) => /[0-9][0-9_]*/,
    float_literal: (_) => /[0-9][0-9_]*\.[0-9_]*/,
    shebang: (_) => seq('#!', /[^\n\r]*/),
    single_line_comment: (_) => seq('//', /.*/),
  }
});
