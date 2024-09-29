module.exports = grammar({
  name: 'tako',

  extras: ($) => [$.nesting_comment, $.single_line_comment, "\r", "\n", "\t", " "],
  rules: {
    // TODO: add the actual grammar rules
    source_file: ($) => seq(optional($.shebang), optional($._body)),
    _body: ($) => seq(repeat($._statement), $._unterminated_statement),
    _statement: ($) => seq(optional($._unterminated_statement), ';'),
    _unterminated_statement: ($) => choice(
      $.definition,
      $._block,
      $.expression
    ),
    _block: ($) => seq('{', optional($._body), '}'),
    definition: ($) => seq($.ident, '=', $.expression),
    expression: ($) => $._inner_expression,
    _inner_expression: ($) => choice(
      seq('(', $.expression ,')'),
      $.string,
      $.number,
      $.hex_literal,
      $.color,
      $.ident
    ),
    number: ($) => choice(
      $.int_literal,
      $.float_literal
    ),
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
    color: ($) => seq('#', choice(
      $._hex_char_3,
      $._hex_char_4,
      $._hex_char_6,
      $._hex_char_8,
    )),
    hex_literal: ($) => seq('0x', /[a-fA-F0-9_]+/),
    _hex_char_3: (_) => /[a-fA-F0-9_]{3}/,
    _hex_char_4: (_) => /[a-fA-F0-9_]{4}/,
    _hex_char_6: (_) => /[a-fA-F0-9_]{6}/,
    _hex_char_8: (_) =>  /[a-fA-F0-9_]{8}/,
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
    // TODO: Add semver.
    shebang: (_) => seq('#!', /[^\n\r]*/),
    single_line_comment: (_) => seq('//', /.*/),
  }
});
