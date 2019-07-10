project('tako', 'cpp',
  version : '1.0.0',
  license : 'MIT')

add_global_arguments('-std=c++17', language : 'cpp')
add_global_arguments('-g', language : 'cpp')

subdir('src')

executable(
  'tako',
  [ 'main.cc' ],
  install : true,
  link_with : tako_lib,
  include_directories : [src_inc, configuration_inc])

subdir('test')
test_exe = executable(
  'tests',
  [ 'test.cc' ],
  link_with : [tako_lib, test_lib])
test('tako_test', test_exe)
