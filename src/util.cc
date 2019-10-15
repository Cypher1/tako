#include "util.h"

bool operator<(const Location &a, const Location &b) {
  if (a.file < b.file) {
    return true;
  }
  if (a.start < b.start) {
    return true;
  }
  return a.length < b.length;
}
