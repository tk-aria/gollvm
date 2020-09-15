//===- llvm/tools/gollvm/unittests/TestUtils/DiffUtils.cpp ----------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//

#include "DiffUtils.h"

#include <assert.h>
#include <iostream>
#include <sstream>

namespace goBackendUnitTests {

DECLARE_EXPECTED_OUTPUT(baseline, R"RAW_RESULT(
    abc
    def
    xyz
)RAW_RESULT");

// macroLineAtStart returns TRUE if the line number reported by the
// compiler for __LINE__ in a macro instantiation points to the start
// of the DECLARE_EXPECTED_OUTPUT macro use, as opposed to the end.
//
// Fun fact: there is some disagreement among C++ compilers about what
// the line is. Some compilers decided that __LINE__ for the macro
// above is the starting line (16) and some decide that it's the
// ending line (20). Which one will matter for the machinery that does
// test remastering.
static bool macroLineAtStart() {
  if (baseline.line == 16) {
    return true;
  } else if (baseline.line == 20) {
    return false;
  } else {
    assert(false && "macroLineAtStart broken -- source edited?");
  }
}

std::string trimsp(const std::string &s) {
  size_t firstsp = s.find_first_not_of(" \n");
  if (firstsp == std::string::npos)
    return s;
  size_t lastsp = s.find_last_not_of(" \n");
  return s.substr(firstsp, (lastsp - firstsp + 1));
}

std::vector<std::string> tokenize(const std::string &s) {
  std::vector<std::string> tokens;

  const std::string del(" \t\n");
  size_t np = s.find_first_not_of(del, 0);
  size_t pos = s.find_first_of(del, np);
  while (pos != std::string::npos || np != std::string::npos) {
    tokens.push_back(s.substr(np, pos - np));
    np = s.find_first_not_of(del, pos);
    pos = s.find_first_of(del, np);
  }
  return tokens;
}

std::string vectostr(const std::vector<std::string> &tv) {
  std::stringstream ss;
  unsigned wc = 0;
  for (auto s : tv)
    ss << " " << wc++ << "[" << s << "]";
  return ss.str();
}

bool difftokens(const std::string &expected, const std::string &result,
                std::string &diffreason)
{
  std::vector<std::string> expv = tokenize(expected);
  std::vector<std::string> resv = tokenize(result);
  unsigned mins = std::min(expv.size(), resv.size());
  unsigned maxs = std::max(expv.size(), resv.size());
  std::stringstream ss;
  bool rval = true;
  if (expv.size() != resv.size()) {
    ss << "lengths differ (" << expv.size() << " vs " << resv.size()
       << ") extra " << (expv.size() > resv.size() ? "result" : "expected")
       << " tokens: ";
    for (unsigned idx = 0; idx < maxs; ++idx) {
      if (idx >= mins)
        ss << (idx < expv.size() ? expv[idx] : resv[idx]) << " ";
    }
    ss << "\n";
    rval = false;
  }
  for (unsigned idx = 0; idx < mins; ++idx) {
    if (expv[idx] != resv[idx]) {
      ss << "token vector diff at slot " << idx << " (expected '" << expv[idx]
         << "' result '" << resv[idx] << "')";
      rval = false;
      break;
    }
  }
  if (! rval)
    diffreason = ss.str();
  return rval;
}

bool containstokens(const std::string &text, const std::string &pat)
{
  std::vector<std::string> textToks = tokenize(text);
  std::vector<std::string> patToks = tokenize(pat);
  for (unsigned ti = 0; ti < textToks.size(); ++ti) {
    bool failed = false;
    for (unsigned tic = ti, pi = 0; pi < patToks.size(); ++pi, ++tic) {
      if (tic >= textToks.size() || patToks[pi] != textToks[tic]) {
        failed = true;
        break;
      }
    }
    if (failed)
      continue;
    return true;
  }
  return false;
}

unsigned countinstances(const std::string &text, const std::string &pat)
{
  unsigned instances = 0;
  std::vector<std::string> textToks = tokenize(text);
  std::vector<std::string> patToks = tokenize(pat);
  for (unsigned ti = 0; ti < textToks.size(); ++ti) {
    bool fail = false;
    for (unsigned tic = ti, pi = 0; pi < patToks.size(); ++pi, ++tic) {
      if (tic >= textToks.size() || patToks[pi] != textToks[tic]) {
        fail = true;
        break;
      }
    }
    if (!fail)
      instances += 1;
  }
  return instances;
}

std::string dumpfilename(const char *tag, unsigned version) {
  std::stringstream ss;
  ss << "/tmp/" << tag << ".dump." << version << ".txt";
  return ss.str();
}

void emitStringToDumpFile(const char *tag,
                          unsigned version,
                          const std::string &payload)
{
  auto fn = dumpfilename(tag, version);
  FILE *fp = fopen(fn.c_str(), "w");
  if (fp) {
    fprintf(fp, "%s\n", payload.c_str());
    fclose(fp);
    std::cerr << "emitted dump file " << fn << "\n";
  }
}

void complainOnNequal(const std::string &reason,
                      const ExpectedDump &ed,
                      const std::string &actual)
{
  bool emitDumpFilesOnDiff = false;
  bool emitRemasterScript = false;

  if (getenv("GOLLVM_UNITTESTS_BACKENDCORE_EMITDUMPFILES") != nullptr)
    emitDumpFilesOnDiff = true;
  if (getenv("GOLLVM_UNITTESTS_BACKENDCORE_EMITDUMPFILES") != nullptr) {
    emitRemasterScript = true;
    emitDumpFilesOnDiff = true;
  }

  std::cerr << reason << "\n";
  const std::string &expected = ed.content;
  std::cerr << "expected dump:\n" << expected << "\n";
  std::cerr << "actual dump:\n" << actual << "\n";
  unsigned mls = macroLineAtStart() ? 1 : 0;
  if (emitDumpFilesOnDiff) {
    static unsigned filecount;
    static FILE *outfp; // script
    emitStringToDumpFile("expected", filecount, expected);
    emitStringToDumpFile("actual", filecount, actual);
    if (emitRemasterScript) {
      // HACK: no explicit close for this file. Assume that it will
      // be closed when the unit test finishes running.
      if (outfp == nullptr) {
        outfp = fopen("/tmp/remaster-inputs.txt", "w");
        fprintf(stderr, "... emitting remaster inputs to file '/tmp/remaster-inputs.txt'\n");
      }
      fprintf(outfp, "%d %s %d %s %s\n", mls, ed.file, ed.line,
              dumpfilename("expected", filecount).c_str(),
              dumpfilename("actual", filecount).c_str());
      fflush(outfp);
    }
    filecount++;
  }
}

} // end namespace
