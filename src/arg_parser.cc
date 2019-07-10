#include <string>
#include <sstream>
#include <iostream>
#include <vector>
#include <unordered_map>

#include "arg_parser.h"

void parseArgs(
  const std::vector<Arg>& args,
  const int start,
  const int argc,
  char* argv[],
  std::vector<std::string>& targets,
  std::unordered_map<std::string, std::string>& values
) {
  const auto setValue = [&values, &args, &argc, &argv](const std::string& name, int& ref) {
      for(const auto& arg : args) {
        if(arg.name != name && std::string("")+arg.flag != name) {
          continue;
        }
        if (!arg.value.size()) {
          values[arg.name] = "";
          return;
        }
        ref++;
        if (ref >= argc) {
          std::cerr << "Option " << arg.name << " needs an argument\n.";
          values["help"] = "";
          return;
        }
        if(!values[arg.name].empty()) {
          throw std::runtime_error("Value for " + arg.name + " overridden.");
        }
        values[arg.name] = argv[ref];
        return;
      }
      throw std::runtime_error("Unexpected argument '" + name + "'.");
  };

  for(int i = start; i < argc; ++i) {
    std::string val = argv[i];
    if(val.size() < 1 || val[0] != '-') {
      targets.push_back(val);
      continue;
    }
    if(val.size() > 1 && val[1] == '-') {
      setValue(val.substr(2), i);
      continue;
    }
    for(size_t n=1; n < val.size(); n++) {
      setValue(val.substr(n, 1), i);
    }
  }
}

std::string makeUsage(
  const std::string& prog,
  const std::vector<Arg>& args,
  const int width
) {
  std::stringstream o;
  o << "Usage: " << prog << " [ options ] <targets>\n";
  for(const auto& arg : args) {
    std::stringstream s;
    s << "  -" << arg.flag;
    if (arg.name.size()) {
      s << " or --" << arg.name;
    }
    if (arg.value.size()) {
      s << " <" << arg.value << ">";
    }
    o << s.str();
    for(int k = s.str().size(); k < width; k++) {
      o << " ";
    }
    o << arg.description << "\n";
  }
  return o.str();
}
