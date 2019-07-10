tests_inc = include_directories('.')

test_sources = [
  'test.cc',
  'checker_tests.cc']

test_exe = executable('test_exe',
                        test_sources,
                        install : true,
                        link_with : tako_lib,
                        include_directories : [src_inc, tests_inc, configuration_inc])

test('tako_test', test_exe)
