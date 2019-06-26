#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <string>
#include "ast.h"
#include "lex.h"
#include "toString.h"

const std::map<TokenType, TokenType> brackets = {
  {TokenType::OpenParen, TokenType::CloseParen},
  {TokenType::OpenBrace, TokenType::CloseBrace},
  {TokenType::OpenBracket, TokenType::CloseBracket},
  {TokenType::SingleQuote, TokenType::SingleQuote},
  {TokenType::DoubleQuote, TokenType::DoubleQuote},
  {TokenType::BackQuote, TokenType::BackQuote},
  {TokenType::Declaration, TokenType::SemiColon},
};

const std::map<TokenType, TokenType> close_brackets = [](){
  std::map<TokenType, TokenType>map;
  for(auto kv : brackets) {
    if(brackets.find(kv.second) == brackets.end()) {
      map.emplace(kv.second, kv.first);
    }
  }
  return map;
}();

const std::map<std::string, int> precedence = {
  {"^", 1},
  {"*", 2},
  {"/", 2},
  {"+", 3},
  {"-", 3},
  {",", 4},
};

constexpr bool isOperator(const TokenType& type) {
  return type == +TokenType::Operator;
}

constexpr bool isQuote(const TokenType& type) {
  return type == +TokenType::SingleQuote || type == +TokenType::DoubleQuote || type == +TokenType::BackQuote;
}

std::vector<Tree<Token>> toArgList(Tree<Token> argTree) {
  std::vector<Tree<Token>> args;

  if(argTree.value.type == +TokenType::Comma) {
    // Add the list of args
    for(auto child : argTree.children) {
      auto childrenExpanded = toArgList(child);
      args.insert(args.end(), childrenExpanded.begin(), childrenExpanded.end());
    }
  } else {
    // Just a node.
    args.push_back(argTree);
  }

  return args;
}

std::vector<Tree<Token>> toExpression(std::vector<Tree<Token>> nodes, Messages& msgs, const std::string& content) {
  if(nodes.empty()) {
    return nodes;
  }
  auto max_op = nodes.end();
  int highest_level = -1; // of weakest precedence
  for(auto it = nodes.begin(); it != nodes.end(); ++it) {
    if(isOperator(it->value.type)) {
      int level = 0;
      const std::string s = getString(it->value.loc, content);
      const auto prec_it = precedence.find(s);
      if(prec_it != precedence.end()) {
        level = prec_it->second;
      }
      if(level > highest_level) {
        max_op = it;
        highest_level = level;
      }
    }
  }

  if(max_op != nodes.end()) {
    // Get the left and right and make them into nodes of this op.
    Tree<Token> curr = *max_op;
    if(nodes.begin() != max_op) {
      std::vector<Tree<Token>> lArgs = toExpression({nodes.begin(), max_op}, msgs, content);
      for(auto lArg : lArgs) {
        const auto ls = toArgList(lArg);
        curr.children.insert(curr.children.end(), ls.begin(), ls.end());
      }
    }

    if(max_op != nodes.end() && max_op+1 != nodes.end()) {
      std::vector<Tree<Token>> rArgs = toExpression({max_op+1, nodes.end()}, msgs, content);
      for(auto rArg : rArgs) {
        const auto rs = toArgList(rArg);
        curr.children.insert(curr.children.end(), rs.begin(), rs.end());
      }
    }
    nodes = {curr};
  }

  if( nodes.size() == 1 && nodes[0].value.type == +TokenType::OpenParen) {
    nodes = nodes[0].children;
  }
  return nodes;
}

std::vector<Tree<Token>> toAst(Tokens& toks, Messages& msgs, const TokenType close, const std::string& content) {
  std::vector<Tree<Token>> children;
  while(toks.size()) {
    Token curr = toks.back();
    const TokenType& type = curr.type;
    const bool inString = isQuote(close);
    if(type == +TokenType::Declaration && !children.empty() && children.back().value.type == +TokenType::Declaration) {
      // This is the end of a set of arguments, we've lost the close bracket.
      break;
    }

    toks.pop_back();

    // Check that this isn't the close.
    if(type == close || (!inString && close_brackets.find(type) != close_brackets.end())) {
      if(type != close && close != +TokenType::SemiColon) {
        // Unbalanced bracket
        msgs.push_back({
          PassStep::Ast,
          MessageType::Error,
          "Unbalanced bracket",
          curr.loc
        });
        curr.type = TokenType::Error;
      }
      break;
    }

    if(!inString) {
      if(type == +TokenType::Comma || type == +TokenType::WhiteSpace) {
        continue;
      }
    }

    std::vector<Tree<Token>> expr;
    // Matching brackets?
    const auto match = brackets.find(type);
    if(match != brackets.end() && (!inString || type == +TokenType::OpenBrace)) {
      expr = toAst(toks, msgs, match->second, content);
      expr = toExpression(expr, msgs, content);
    }

    if(!inString && type == +TokenType::OpenParen && !children.empty() && children.back().value.type == +TokenType::Symbol) {
      if(expr.empty()) {
        msgs.push_back({
            PassStep::Ast,
            MessageType::Info,
            "No need for the parentheses '()' here.",
            curr.loc
        });
      } else {
        // The previous symbol is a function call, these are the arguments
        auto end = children.back().children.end();
        children.back().children.insert(end, expr.begin(), expr.end());
      }
      continue;
    }
    children.push_back({curr, expr});
  }

  return children;
}

Tree<Token> ast(Tokens& toks, Messages& msgs, const std::string& content, const std::string& filename) {
  std::vector<Tree<Token>> all_nodes;
  while(!toks.empty()) {
    auto nodes = toAst(toks, msgs, TokenType::Error, content);
    all_nodes.insert(all_nodes.end(), nodes.begin(), nodes.end());
  }
  return {{TokenType::Symbol, {0, 0, filename}}, all_nodes};
}
