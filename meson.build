project('tako', 'cpp',
  version : '1.0.0',
  license : 'MIT')

add_global_arguments('-std=c++17', language : 'cpp')
add_global_arguments('-g', language : 'cpp')

conf_data = configuration_data()
# Set data
conf_data.set('version', meson.project_version())

subdir('src')

executable(
  'tako',
  [ 'main.cc' ],
  install : true,
  link_with : tako_lib,
  include_directories : [src_inc])

subdir('test')
