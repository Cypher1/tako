add_library(checker checker.cc ../util/context.cc)
target_link_libraries(checker parser)
