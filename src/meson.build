conf_data = configuration_data()
# Set data
conf_data.set('version', '1.0.0')

configure_file(input : 'takoConfig.h.in',
  output : 'takoConfig.h',
  configuration : conf_data)

configuration_inc = include_directories('.')

src_inc = include_directories('.')
tako_sources = [
  'arg_parser.cc',
  'ast.cc',
  'checker.cc',
  'context.cc',
  'lex.cc',
  'parser.cc',
  'toString.cc',
  'util.cc']

tako_lib = static_library(
  'tako_lib',
  tako_sources,
  include_directories : [src_inc, configuration_inc])
