//===- Tokenizertests.cpp -------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "macro-tokenizer.h"
#include "gtest/gtest.h"

#include "DiffUtils.h"

using namespace goBackendUnitTests;

namespace {

static std::map<MacTokenTyp, const char *> m =
{  { TOK_END_OF_STRING, "<eos>" },
   { TOK_IDENTIFIER, "IDENTIFIER" },
   { TOK_NUMERIC_CONSTANT, "NCONST" },
   { TOK_STRING_CONSTANT, "SCONST" },
   { TOK_OPEN_PAREN, "OPEN_PAREN" },
   { TOK_CLOSE_PAREN, "CLOSE_PAREN" },
   { TOK_ADDSUB, "ADDSUB" },
   { TOK_BINOP, "BINOP" },
   { TOK_UNOP, "UNOP" },
   { TOK_SPACE, "SPACE" },
   { TOK_ERROR, "ERROR" } };

std::string dump(MacroTokenizer &t)
{
  std::stringstream ss;

  while(1) {
    auto pair = t.getToken();
    if (pair.first == TOK_END_OF_STRING)
      return ss.str();
    auto it = m.find(pair.first);
    if (it != m.end()) {
      ss << it->second;
    } else {
      ss << "unknown(" << ((int)pair.first) << ")";
    }
    ss << " '" << pair.second << "'\n";
    // Stop at first error
    if (pair.first == TOK_ERROR)
      break;
  }
  return ss.str();
}

bool expectTokens(MacroTokenizer &t, const std::string &expected)
{
  std::string reason;
  std::string actual(dump(t));
  bool equal = difftokens(expected, actual, reason);
  if (! equal)
    complainOnNequal(reason, expected, actual, false);
  return equal;
}

TEST(GoDumpSpecTokenizexortests, BasicTokenizer) {
  std::string input("1 2l .3f");
  MacroTokenizer t(input);

  const char *exp = R"RAW_RESULT(
    NCONST '1'
    SPACE ' '
    NCONST '2'
    SPACE ' '
    NCONST '.3'
    )RAW_RESULT";

  bool isOK = expectTokens(t, exp);
  EXPECT_TRUE(isOK);
}

TEST(GoDumpSpecTokenizertests, StringConstantTokens) {
  std::string input("\"s1\" '\\t\\n\\'' \"internal \\\" here\"");
  MacroTokenizer t(input);

  const char *exp = R"RAW_RESULT(
    SCONST '"s1"'
    SPACE ' '
    SCONST ''\t\n\'''
    SPACE ' '
    SCONST '"internal \" here"'
    )RAW_RESULT";

  bool isOK = expectTokens(t, exp);
  EXPECT_TRUE(isOK);
}

TEST(GoDumpSpecTokenizertests, NumericConstantTokens) {
  std::string input(std::string("2.898e-1 0xdeadbeef9 101ull"));
  MacroTokenizer t(input);

  const char *exp = R"RAW_RESULT(
    NCONST '2.898e'
    ADDSUB '-'
    NCONST '1'
    SPACE ' '
    NCONST '0xdeadbeef9'
    SPACE ' '
    NCONST '101'
    )RAW_RESULT";

  bool isOK = expectTokens(t, exp);
  EXPECT_TRUE(isOK);
}

TEST(GoDumpSpecTokenizertests, MiscOps) {
  std::string input(std::string("1(2/3)%3*A==1<<9>Q"));
  MacroTokenizer t(input);

  const char *exp = R"RAW_RESULT(
    NCONST '1'
    OPEN_PAREN '('
    NCONST '2'
    BINOP '/'
    NCONST '3'
    CLOSE_PAREN ')'
    BINOP '%'
    NCONST '3'
    BINOP '*'
    IDENTIFIER 'A'
    BINOP '=='
    NCONST '1'
    BINOP '<<'
    NCONST '9'
    BINOP '>'
    IDENTIFIER 'Q'
    )RAW_RESULT";

  bool isOK = expectTokens(t, exp);
  EXPECT_TRUE(isOK);
}
TEST(GoDumpSpecTokenizertests, MoreOps) {
  std::string input(std::string("1|2&A^z!/!=~F"));
  MacroTokenizer t(input);

  const char *exp = R"RAW_RESULT(
    NCONST '1'
    BINOP '|'
    NCONST '2'
    BINOP '&'
    IDENTIFIER 'A'
    BINOP '^'
    IDENTIFIER 'z'
    UNOP '!'
    BINOP '/'
    BINOP '!='
    UNOP '^'
    IDENTIFIER 'F'
    )RAW_RESULT";

  bool isOK = expectTokens(t, exp);
  EXPECT_TRUE(isOK);
}

TEST(GoDumpSpecTokenizertests, ErrorTokenizer) {
  std::string input(std::string("A=1"));
  MacroTokenizer t(input);

  const char *exp = R"RAW_RESULT(
    IDENTIFIER 'A'
    ERROR '='
    )RAW_RESULT";

  bool isOK = expectTokens(t, exp);
  EXPECT_TRUE(isOK);
}

} // namespace
