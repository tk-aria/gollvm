//===- llvm/tools/gollvm/unittests/TestUtils/DiffUtils.h --------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//

#ifndef GOLLVM_UNITTESTS_TESTUTILS_DIFFUTILS_H
#define GOLLVM_UNITTESTS_TESTUTILS_DIFFUTILS_H

#include <string>
#include <vector>

#define RAW_RESULT(x) #x
#define RAW_INPUT(x) #x

struct ExpectedDump {
  ExpectedDump(const char *c, const char *f, int l) :
      content(c), file(f), line(l) { }
  const char *content;
  const char *file;
  int line;
};

#define DECLARE_EXPECTED_OUTPUT(vname, x) \
  ExpectedDump vname(x, __FILE__, __LINE__)

namespace goBackendUnitTests {

// Trim leading and trailing spaces
std::string trimsp(const std::string &s);

// Split specified string into tokens (with whitespace as delimiter)
std::vector<std::string> tokenize(const std::string &s);

// Join together vector of strings to single string, separate with spaces
std::string vectostr(const std::vector<std::string> &tv);

// Tokenize the two strings, then diff the resulting token vectors,
// returning TRUE if they are identical or FALSE if different (and
// setting 'diffreason' to explanation of diff)
bool difftokens(const std::string &expected,
                const std::string &result,
                std::string &diffreason);

// Return TRUE if string 'text' contains instead of string 'pat'.
// Tokenizes both strings to avoid whitespace differences
bool containstokens(const std::string &text, const std::string &pat);

// Return the number of instances of 'pat' within 'text'. Both the pattern
// and the text are tokenized prior to the search. Search is brute-force.
unsigned countinstances(const std::string &text, const std::string &pat);

// Issue an error message to std:cerr to report the fact that
// 'expected' does not match 'actual'; emit actual/expected debug dump
// files if 'emitDump' is true.
void complainOnNequal(const std::string &reason,
                      const std::string &expected,
                      const std::string &actual,
                      bool emitDump);

}

#endif // GOLLVM_UNITTESTS_TESTUTILS_DIFFUTILS_H
