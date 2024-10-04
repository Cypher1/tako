// For now we match rust.
// https://doc.rust-lang.org/reference/expressions.html#expression-precedence
const {left, right} = prec;
const PREC = {
  call: 17,
  field: 16,
  try: 15,
  neg: 14,
  not: 14,
  bit_not: 14,
  cast: 13,
  mul: 12,
  div: 12,
  mod: 12,
  add: 11,
  sub: 11,
  left_shift: 10,
  right_shift: 10,
  bit_and: 9,
  bit_xor: 8,
  bit_or: 7,
  // comparative: 6,
  equals: 6,
  not_equals: 6,
  less_than: 6,
  less_than_equals: 6,
  greater_than: 6,
  greater_than_equals: 6,
  and: 5,
  or: 4,
  range: 3,
  has_type: 2,
  assign: 0,
  closure: -2,
  sequence: -3,
};

const RIGHT_OPERATORS = [
  ['assign', '='],
];

const OPERATORS = [
  ['field', '.'],
  ['has_type', ':'],
  ['and', '&&'],
  ['or', '||'],
  ['bit_and', '&'],
  ['bit_or', '|'],
  ['bit_xor', '^'],
  ['equals', '=='],
  ['not_equals', '!='],
  ['less_than', '<'],
  ['less_than_equals', '<='],
  ['greater_than', '>'],
  ['greater_than_equals', '>='],
  ['left_shift', '<<'],
  ['right_shift', '>>'],
  ['add', '+'],
  ['sub', '-'],
  ['mul', '*'],
  ['div', '/'],
  ['mod', '%'],
];

const POSTFIX_OPERATORS = [
  ['try', '?'],
];

const OPTIONALLY_POSTFIX_OPERATORS = [
  ['sequence', ';'],
];

const UNARY_OPERATORS = [
  ['neg', '-'],
  ['not', '!'],
  ['bit_not', '~'],
];

const ALL_OPERATORS = [
  ...OPERATORS,
  ...RIGHT_OPERATORS,
  ...POSTFIX_OPERATORS,
  ...OPTIONALLY_POSTFIX_OPERATORS,
  ...UNARY_OPERATORS,
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
    operators[name] = ($) => left(precedence, seq(
      field('left', $._expression),
      field('operator', operator),
      field('right', $._expression),
    ));
  }
  for (const [name, operator] of RIGHT_OPERATORS) {
    const precedence = PREC[name];
    operators[name] = ($) => right(precedence, seq(
      field('left', $._expression),
      field('operator', operator),
      field('right', $._expression),
    ));
  }
  for (const [name, operator] of OPTIONALLY_POSTFIX_OPERATORS) {
    const precedence = PREC[name];
    operators[name] = ($) => right(precedence, seq(
      field('left', $._expression),
      field('operator', operator),
      field('right', optional($._expression)),
    ));
  }
  for (const [name, operator] of POSTFIX_OPERATORS) {
    const precedence = PREC[name];
    operators[name] = ($) => left(precedence, seq(
      field('left', $._expression),
      field('operator', operator),
    ));
  }
  for (const [name, operator] of UNARY_OPERATORS) {
    const precedence = PREC[name];
    operators[name] = ($) => right(precedence, seq(
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
    source_file: ($) => seq(optional($.shebang), separated_one(optional($._expression), $.heading)),
    _expression: ($) => choice(
      $._expression_not_literal,
      $.string_literal,
      $._number,
      $.hex_literal,
      $.color,
    ),
    _expression_not_literal: ($) => choice(
      $._operator_expression, // Consider keeping this name to support editing?
      $.parens,
      $.container,
      $.call,
      $.block,
      $.ident,
    ),
    block: ($) => seq('{', optional($._expression), '}'),
    parens: ($) => seq('(', $._expression ,')'),
    container: ($) => seq('[', separated($._expression, ','), optional(',') ,']'),
    call: ($) => left(PREC.call, seq($._expression_not_literal, '(', separated($._expression, ','), optional(','), ')')),
    _operator_expression: ($) => {
      return choice(...ALL_OPERATORS.map(([name, _operator_parser]) => {
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
    string_literal: $ => choice(
      seq(
        '\'',
        repeat(choice(
          $.escape_sequence,
          /[^\']/,
        )),
        token.immediate('\''),
      ),
      seq(
        '"',
        repeat(choice(
          $.escape_sequence,
          /[^\"]/,
        )),
        token.immediate('"'),
      ),
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
    heading: (_) => /====*[^='"]*====*/,
    ...operators_gen(),
  }
});
