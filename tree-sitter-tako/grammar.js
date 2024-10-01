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
  comma: -1,
  has_type: -2,
  closure: -3,
};

const OPERATORS = [
  ['comma', ','],
  ['assign', '='],
  ['has_type', ':'],
  ['and', '&&'],
  ['or', '||'],
  ['bitand', '&'],
  ['bitor', '|'],
  ['bitxor', '^'],
  ['comparative', choice('==', '!=', '<', '<=', '>', '>=')],
  ['shift', choice('<<', '>>')],
  ['additive', choice('+', '-')],
  ['multiplicative', choice('*', '/', '%')],
];

const separated_one = (entry, delimiter) => {
  return seq(entry, repeat(seq(delimiter, entry)));
};

const separated = (entry, delimiter) => {
  return optional(separated_one(entry, delimiter));
};

function operators_gen() {
  const operators = {};
  for (const [name, operator] of OPERATORS) {
    const precedence = PREC[name];
    operators[name] = ($) => prec.left(precedence, seq(
      field('left', $._expression),
      // @ts-ignore
      field('operator', operator),
      field('right', $._expression),
    ));
  }
  return operators;
}

module.exports = grammar({
  name: 'tako',
  extras: ($) => [$.nesting_comment, $.single_line_comment, "\r", "\n", "\t", " "],
  rules: {
    // TODO: add the actual grammar rules
    source_file: ($) => seq(optional($.shebang), optional($._non_empty_body)),
    _non_empty_body: ($) => separated_one($._statement, ';'),
    _statement: ($) => choice(
      $.block,
      $._expression
    ),
    block: ($) => seq('{', optional($._non_empty_body), '}'),
    _expression: ($) => choice(
      $._binary_expression, // Consider keeping this name to support editing?
      $.call,
      seq('(', $._expression ,')'),
      $.string_literal,
      $._number,
      $.hex_literal,
      $.color,
      $.ident,
    ),
    call: ($) => seq($._expression, '(', separated($._expression, ','), optional(','), ')'),
    _binary_expression: ($) => {
      return choice(...OPERATORS.map(([name, _operator_parser]) => {
        try {
          return ($)[name]; // Get the parser 'named'.
        } catch (e) {
          e.message = `OPERATOR: ${name} ('${operator}') PRECEDENCE: '${precedence}'.\n${e.message}`;
          throw e;
        }
      }));
    },
    _number: ($) => choice(
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
    ...operators_gen(),
  }
});
