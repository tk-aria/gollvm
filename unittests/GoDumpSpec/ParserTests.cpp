//===---- ParserTests.cpp -------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "macro-parser.h"
#include "gtest/gtest.h"

#include "DiffUtils.h"

using namespace goBackendUnitTests;

namespace {

void parseMacros(MacroParser &parser, const char *input)
{
  // Feed lines from the input to the parser, skipping blanks
  assert(input != nullptr);
  std::stringstream ss(input);
  std::string line;
  unsigned lno = 1;
  while (std::getline(ss,line,'\n')) {
    if (line.empty())
      continue;
    auto fne = line.find_first_not_of(" \t");
    if (fne == std::string::npos)
      continue;
    parser.visitMacroLine(line.substr(fne), lno++);
  }
}

std::string postProcessAndEmit(MacroParser &parser)
{
  // Post-process
  parser.postProcessMacros();

  // Emit result to a string and return
  std::string result;
  llvm::raw_string_ostream os(result);
  std::unordered_set<std::string> empty;
  parser.emitMacros(os, empty);
  return os.str();
}

std::string processMacros(const char *input)
{
  MacroParser parser;
  parseMacros(parser, input);
  return postProcessAndEmit(parser);
}

bool expectEqualTokens(const std::string &actual, const std::string &expected)
{
  std::string reason;
  bool equal = difftokens(expected, actual, reason);
  if (! equal)
    complainOnNequal(reason, expected, actual, false);
  return equal;
}

TEST(GoDumpSpecParserTests, BasicParser) {

  const char *input = R"RAW_RESULT(
    #define FOO BAR
    #define BLIX
    #define BAR 10
    )RAW_RESULT";
  EXPECT_TRUE(true);

  std::string result = processMacros(input);

  const char *exp = R"RAW_RESULT(
    const _BAR = 10
    const _FOO = _BAR
    )RAW_RESULT";

  EXPECT_TRUE(expectEqualTokens(result, exp));
}

TEST(GoDumpSpecParserTests, MacroDefCycle) {

  const char *input = R"RAW_RESULT(
    #define FOO BAR
    #define BLIX "hello world"
    #define BAR FOO
    )RAW_RESULT";
  EXPECT_TRUE(true);

  std::string result = processMacros(input);

  const char *exp = R"RAW_RESULT(
    const _BLIX = "hello world"
    )RAW_RESULT";

  EXPECT_TRUE(expectEqualTokens(result, exp));
}

TEST(GoDumpSpecParserTests, FunctionMacros) {


  const char *input = R"RAW_RESULT(
    #define FOO(x,y) x+y
    #define BLIX (7.8e-2*9.01e-8+.00001)<<4u
    #define GLIX() FOO(3,4)
    )RAW_RESULT";
  EXPECT_TRUE(true);

  std::string result = processMacros(input);

  const char *exp = R"RAW_RESULT(
    const _BLIX = (7.8e-2*9.01e-8+.00001)<<4
    )RAW_RESULT";

  EXPECT_TRUE(expectEqualTokens(result, exp));
}

TEST(GoDumpSpecParserTests, BadStringLiterals) {

  const char *input = R"RAW_RESULT(
    #define BAD1 '\0'
    #define BAD2 "ok up until this: \0 "
    #define BAD3 '\xa'
    #define GOOD ""
    )RAW_RESULT";
  EXPECT_TRUE(true);

  std::string result = processMacros(input);

  const char *exp = R"RAW_RESULT(
    const _GOOD = ""
    )RAW_RESULT";

  EXPECT_TRUE(expectEqualTokens(result, exp));
}

TEST(GoDumpSpecParserTests, EmptyMacros) {

  const char *input = R"RAW_RESULT(
    #define EMPTY1
    #define EMPTY2 EMPTY1
    #define EMPTY3 EMPTY2 EMPTY1
    #define BAD (1+2)<<
    #define GOOD (1+2)<<9
    )RAW_RESULT";
  EXPECT_TRUE(true);

  std::string result = processMacros(input);

  const char *exp = R"RAW_RESULT(
    const _GOOD = (1+2)<<9
    )RAW_RESULT";

  EXPECT_TRUE(expectEqualTokens(result, exp));
}

TEST(GoDumpSpecParserTests, MacrosAndEnums) {

  const char *input = R"RAW_RESULT(
    #define E1 10
    #define E2 (EX + E1)
    #define EX "ignored"
    )RAW_RESULT";
  EXPECT_TRUE(true);

  MacroParser parser;

  // Register enum
  parser.addEnumLiteralPseudoMacro("EX", "9");

  // Now digest macros
  parseMacros(parser, input);

  std::string result = postProcessAndEmit(parser);

  const char *exp = R"RAW_RESULT(
    const _E1 = 10
    const _E2 = (_EX + _E1)
    const _EX = 9
    )RAW_RESULT";

  EXPECT_TRUE(expectEqualTokens(result, exp));
}

} // namespace
