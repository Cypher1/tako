configure_file(input : 'takoConfig.h.in',
  output : 'takoConfig.h',
  configuration : conf_data)

src_inc = include_directories('.')
tako_sources = [
  'arg_parser.cc',
  'ast.cc',
  'checker.cc',
  'context.cc',
  'eval.cc',
  'lex.cc',
  'parser.cc',
  'show.cc']

tako_lib = static_library(
  'tako_lib',
  tako_sources,
  include_directories : [src_inc])
