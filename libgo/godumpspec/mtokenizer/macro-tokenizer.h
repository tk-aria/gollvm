//===-- macro-tokenizer.h - tokenizer helper for godumpspec ---------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Defines MacroTokenizer class
//
//===----------------------------------------------------------------------===//

#ifndef MACRO_TOKENIZER_H
#define MACRO_TOKENIZER_H

#include <assert.h>
#include <string>
#include <sstream>

typedef enum {
  TOK_END_OF_STRING,
  TOK_IDENTIFIER,
  TOK_NUMERIC_CONSTANT,
  TOK_STRING_CONSTANT,
  TOK_OPEN_PAREN,
  TOK_CLOSE_PAREN,
  TOK_ADDSUB,
  TOK_BINOP,
  TOK_UNOP,
  TOK_SPACE,
  TOK_ERROR
} MacTokenTyp;

// This helper class is used as part of the macro parsing code within
// llvm-godumpspec. Given a string presumably corresponding to the
// right hand side (body) of a macro, it walks through the string and
// breaks it up into tokens (identifiers, constants, parens, etc)
// which are then returned to the client via a 'getToken' interface.
// See the associated unit test for examples.

class MacroTokenizer {
 public:
  // Initialize tokenizer with a given input string string.
  explicit MacroTokenizer(const std::string &ins)
      : in_(ins),
        pos_(0u),
        len_(ins.size())
  {
  }

  // Return next token from string (or TOK_END_OF_STRING if we run out).
  std::pair<MacTokenTyp, std::string> getToken();

 private:
  explicit MacroTokenizer(const char *str);

  bool done() {
    return pos_ >= len_;
  }

  char cur() const {
    return in_[pos_];
  }
  char next(unsigned k) const {
    if (pos_ + k >= len_)
      return '\0';
    return in_[pos_+k];
  }
  char prev(unsigned k) const {
    if (k > pos_ || pos_-k >= len_)
      return '\0';
    return in_[pos_-k];
  }

  std::string consume1() {
    std::string r;
    r += cur();
    pos_ += 1;
    return r;
  }

  void consume1ss(std::stringstream &ss) {
    ss << cur();
    pos_ += 1;
  }

  std::string consumeN(unsigned n) {
    assert(pos_ + n < len_);
    std::string r(in_.substr(pos_, n));
    pos_ += n;
    return r;
  }

  template<typename TestCharFcn>
  std::string consume(TestCharFcn testchar)
  {
    std::string result;
    assert(!done());
    assert(testchar(cur()));
    while (!done() && testchar(cur())) {
      result += cur();
      pos_ += 1;
    }
    return result;
  }

 private:
  const std::string &in_;
  std::string out_;
  unsigned pos_;
  unsigned len_;
};

#endif // MACRO_TOKENIZER_H
