tests_inc = include_directories('.')

test_sources = [
  'checker_tests.cc']

test_lib = static_library(
  'test_lib',
  test_sources,
  link_with : [tako_lib],
  include_directories : [src_inc, tests_inc])

checker_tests = executable(
  'checker_tests', [ 'checker_tests.cc' ],
  link_with : [tako_lib])
test('checker_tests', checker_tests)

parser_tests = executable(
  'parser_tests', [ 'parser_tests.cc' ],
  link_with : [tako_lib])
test('parser_tests', parser_tests)

eval_tests = executable(
  'eval_tests', [ 'eval_tests.cc' ],
  link_with : [tako_lib])
test('eval_tests', eval_tests)
