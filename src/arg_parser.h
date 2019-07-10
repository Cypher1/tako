#pragma once
#ifndef ARG_PARSER_H
#define ARG_PARSER_H

#include <vector>
#include <string>

struct Arg {
  char flag;
  std::string name;
  std::string description;
  std::string value;
};

void parseArgs(
  const std::vector<Arg>& args,
  const int start,
  const int argc,
  char* argv[],
  std::vector<std::string>& targets,
  std::unordered_map<std::string, std::string>& values
);

std::string makeUsage(const std::string& prog, const std::vector<Arg>& args, const int width=22);

#endif // #ifndef ARG_PARSER_H
