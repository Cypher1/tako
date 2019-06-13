#include <vector>
#include <string>

enum class TokenType {
  OpenParen,
  CloseParen,
  OpenBrace,
  CloseBrace,
  PreCond,
  PostCond,
  Definition,
  Quote,
  Symbol
};

using Position = unsigned int;
using Offset = unsigned int;

struct Location {
  Position start;
  Offset length;
  std::string file;
};

struct Token {
  TokenType type;
  Location loc;
};

using Tokens = std::vector<Token>;

enum class MessageLevel {
  Info,
  Warning,
  Error,
  Failure
};

struct Message {
  MessageLevel level;
  std::string msg;
  Location loc;
};

using Messages = std::vector<Message>;

template<typename T>
struct Result {
  T value;
  Messages errors;
};

template<typename T>
struct Node {
  T value;
  Node* parent;
  std::vector<Node> children;
};

Result<Tokens> lex(std::string filename, std::string content);

