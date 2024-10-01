// For now we match rust.
// https://doc.rust-lang.org/reference/expressions.html#expression-precedence
const PREC = {
  call: 15,
  field: 14,
  try: 13,
  unary: 12,
  cast: 11,
  multiplicative: 10,
  additive: 9,
  shift: 8,
  bitand: 7,
  bitxor: 6,
  bitor: 5,
  comparative: 4,
  and: 3,
  or: 2,
  range: 1,
  assign: 0,
  closure: -1,
};

const OPERATORS = [
  [PREC.and, '&&'],
  [PREC.or, '||'],
  [PREC.bitand, '&'],
  [PREC.bitor, '|'],
  [PREC.bitxor, '^'],
  [PREC.comparative, choice('==', '!=', '<', '<=', '>', '>=')],
  [PREC.shift, choice('<<', '>>')],
  [PREC.additive, choice('+', '-')],
  [PREC.multiplicative, choice('*', '/', '%')],
];

const separated_one = (entry, delimiter) => {
  return seq(entry, repeat(seq(delimiter, entry)));
};

const separated = (entry, delimiter) => {
  return optional(separated_one(entry, delimiter));
};

module.exports = grammar({
  name: 'tako',

  extras: ($) => [$.nesting_comment, $.single_line_comment, "\r", "\n", "\t", " "],
  rules: {
    // TODO: add the actual grammar rules
    source_file: ($) => seq(optional($.shebang), optional($._non_empty_body)),
    _non_empty_body: ($) => separated_one($._statement, ';'),
    _statement: ($) => choice(
      $.block,
      $.definition,
      $._expression
    ),
    block: ($) => seq('{', optional($._non_empty_body), '}'),
    definition_target: ($) => seq($.ident, optional($.definition_arguments)),
    definition_arguments: ($) => seq('(', separated($.argument_definition, ','), optional(','), ')'),
    argument_definition: ($) => seq($.definition_target, optional(seq('=', $._expression))),
    definition: ($) => seq($.definition_target, '=', $._expression),
    _expression: ($) => choice(
      $.binary_expression,
      seq('(', $._expression ,')'),
      $.string_literal,
      $.number,
      $.hex_literal,
      $.color,
      $.ident
    ),
    binary_expression: ($) => {
      return choice(...OPERATORS.map(([precedence, operator]) => prec.left(precedence, seq(
        field('left', $._expression),
        // @ts-ignore
        field('operator', operator),
        field('right', $._expression),
      ))));
    },
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
      '*/' // This has to be immediately after the contents.
    ),
    _anything: (_) => token.immediate(/[^*/]|\*[^/]|\/[^*]/),
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
    hex_literal: (_) => seq('0x', /[a-fA-F0-9_]+/),
    _hex_char_3: (_) => /[a-fA-F0-9_]{3}/,
    _hex_char_4: (_) => /[a-fA-F0-9_]{4}/,
    _hex_char_6: (_) => /[a-fA-F0-9_]{6}/,
    _hex_char_8: (_) =>  /[a-fA-F0-9_]{8}/,
    ident: (_) => /[a-zA-Z][a-zA-Z0-9_]*/,
    string_literal: $ => seq(
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
