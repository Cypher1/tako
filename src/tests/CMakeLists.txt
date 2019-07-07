# include our deps
add_subdirectory("lib/rapidcheck")

add_library(tests checker_tests.cc)
target_link_libraries(tests rapidcheck)
