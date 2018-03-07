//===-- macro-parser.h - parser helper for godumpspec ---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Declaration MacroParser class.
//
//===----------------------------------------------------------------------===//

#ifndef MACRO_PARSER_H
#define MACRO_PARSER_H

#include <assert.h>
#include <string>
#include <unordered_set>

#include "macro-tokenizer.h"
#include "llvm/Support/raw_ostream.h"

// Stores the state of a given macro def; used during post-processing
// to detect cycles and to record the final disposition of a macro.
typedef enum {
  Unvisited,
  VisitInProgress,
  VisitedWithError,
  VisitedEmpty,
  VisitedOK
} MacVisitState;

// Container for a macro definition.

struct MacroDef {
  MacroDef() : mstate(Unvisited) { }
  explicit MacroDef(const std::string &n) : name(n), mstate(Unvisited) {}

  std::string name;
  std::string body;
  std::string expanded; // filled in during post-processing
  MacVisitState mstate;
  bool enumDef;         // pseudo-macro created from enum literal
};

// Helper class to parse and post-process a collection of macros,
// notably the output of a "cc -E -dM <file>.c" compile. Expected use
// is that the client will invoke 'visitMacroLine' for each line read
// from a ""cc -E -dM" dump, then invoke 'postProcessMacros' and
// finally "emitMacros", which will emit equivalent Go constants for
// the macros.
//
// Since we're only interested in macros that can be translated into
// Go constants, the parser ignores function macros (ex: "#define foo(x) x+1")
// and other more elaborate/complex macro constructs. Macros with cycles
// are detected and not emitted, similarly macros containing references
// to other non-macro things (function calls, variables, etc).
//
// Macros are allowed to refer to enum literals, since this is a common
// usage in C; this is handled by allowing the client to pre-populate
// the macro table with definitions corresponding to enum literals.

class MacroParser {
 public:
  MacroParser() { }
  int visitMacroLine(const std::string &line, unsigned lno);
  void addEnumLiteralPseudoMacro(const std::string &name,
                                 const std::string &value);
  void postProcessMacros();

  // Emit Go versions of the macros parsed so far to output stream
  // "os". If a macro happens to have the same name as a previously
  // emitted Go type (specified in 'emittedTypeNames') then do not
  // emit that macro definition (types are given precedence over
  // macros here).
  void emitMacros(llvm::raw_ostream &os,
                  const std::unordered_set<std::string> &emittedTypeNames);

 private:
  void visitMacro(MacroDef *m);

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
