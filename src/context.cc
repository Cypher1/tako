#include "context.h"
#include "util.h"
#include <stdexcept>

bool Context::done() {
  return step == final;
}

void Context::startStep(PassStep start_step) {
  step = start_step;
}

PassStep Context::getStep() {
  return step;
}

void Context::msg(Location loc, MessageType level, std::string msg_text) {
  msgs.push_back({step, level, msg_text, loc});
}

std::string Context::getStringAt(const Location &loc) const {
  if(loc.start + loc.length >= content.size()) {
    return "";
  }
  return content.substr(loc.start, loc.length);
}

Messages Context::getMsgs() const {
  return msgs;
}
