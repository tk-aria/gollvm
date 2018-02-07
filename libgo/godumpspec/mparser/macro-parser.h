//===-- macro-parser.h - parser helper for godumpspec' ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Defines MacroParser class
//
//===----------------------------------------------------------------------===//

#ifndef MACRO_PARSER_H
#define MACRO_PARSER_H

#include <assert.h>
#include <string>
#include <unordered_set>

#include "macro-tokenizer.h"
#include "llvm/Support/raw_ostream.h"

typedef enum {
  Unvisited,
  VisitInProgress,
  VisitedWithError,
  VisitedEmpty,
  VisitedOK
} MacVisitState;

struct MacroDef {
  MacroDef() : mstate(Unvisited) { }
  explicit MacroDef(const std::string &n) : name(n), mstate(VisitedEmpty) {}
  std::string name;
  std::string body;
  std::string expanded;
  MacVisitState mstate;
};

class MacroParser {
 public:
  MacroParser() { }
  int visitMacroLine(const std::string &line, unsigned lno);
  void postProcessMacros();

  void emitMacros(llvm::raw_ostream &os,
                  const std::unordered_set<std::string> &emittedTypeNames);

 private:
  void expandMacro(MacroDef *m);

  MacroDef *lookup(const std::string &name) {
    MacroDef t(name);
    auto it = macros_.find(t);
    if (it != macros_.end())
      return const_cast<MacroDef*>(&(*it));
    return nullptr;
  }

  class mdef_hash {
   public:
    unsigned int operator()(const MacroDef &d) const {
      return std::hash<std::string>{}(d.name);
    }
  };

  class mdef_equal {
   public:
    bool operator()(const MacroDef &d1, const MacroDef &d2) const {
      return d1.name.compare(d2.name) == 0;
    }
  };

 private:
  std::unordered_set<MacroDef, mdef_hash, mdef_equal> macros_;
};

#endif // MACRO_PARSER_H
