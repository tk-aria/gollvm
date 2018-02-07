//===-- macro-parser.cpp - definitions for godumpsecp parser --------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Definitions for MacroParser class
//
//===----------------------------------------------------------------------===//

#include "macro-parser.h"

#include <iostream>
#include <vector>
#include <algorithm>

int MacroParser::visitMacroLine(const std::string &line, unsigned lno)
{
  // Chop off the initial "#define "
  if (std::strncmp(line.c_str(), "#define ", 8) != 0) {
    llvm::errs() << "malformed macro input at line "
                 << lno << ": " << line << "\n";
    return 1;
  }
  std::string mac(line.substr(8));
  auto spos = mac.find(' ');
  if (spos == std::string::npos) {
    llvm::errs() << "malformed macro input at line "
                 << lno << ": " << line << "\n";
    return 1;
  }

  // Divide into macro name and macro body
  std::string macname(mac.substr(0, spos));
  std::string macbody(mac.substr(spos+1));

  // Add entry to macro table for later post-processing
  MacroDef d;
  d.name = macname;
  d.body = macbody;
  assert(macros_.find(d) == macros_.end());
  macros_.insert(d);

  return 0;
}

void MacroParser::expandMacro(MacroDef *m)
{
  switch(m->mstate) {
    case VisitInProgress:
      // Cycles not allowed.
      m->mstate = VisitedWithError;
      return;
    case VisitedOK:
    case VisitedEmpty:
    case VisitedWithError:
      // already processed
      return;
    case Unvisited:
      m->mstate = VisitInProgress;
      break;
    default:
      assert(false);
  }

  MacroTokenizer t(m->body);
  std::string str;
  llvm::raw_string_ostream os(str);

  bool eof = false;
  bool error = false;
  bool saw_operand = false;
  bool expect_operand = false;

  while(!error && !eof) {
    std::pair<MacTokenTyp, std::string> tv = t.getToken();
      switch(tv.first) {
        case TOK_ERROR:
          error = true;
          break;
        case TOK_END_OF_STRING:
          eof = true;
          break;
        case TOK_IDENTIFIER: {
          if (saw_operand) {
            error = true;
            break;
          }
          MacroDef *m2 = lookup(tv.second);
          if (m2 == nullptr) {
            // Not a macro -- bail here
            error = true;
            break;
          }
          expandMacro(m2);
          if (m2->mstate != VisitedOK) {
            error = true;
            break;
          }
          if (m2->mstate == VisitedEmpty) {
            // Don't emit references to empty macros.
          } else {
            os << "_" << m2->name;
            saw_operand = true;
            expect_operand = false;
          }
          break;
        }
        case TOK_NUMERIC_CONSTANT:
          os << tv.second;
          saw_operand = true;
          expect_operand = false;
          break;
        case TOK_SPACE:
          os << tv.second;
          break;
        case TOK_ADDSUB:
          saw_operand = false;
          os << tv.second;
          break;
        case TOK_UNOP:
          if (saw_operand) {
            error = true;
            break;
          }
          expect_operand = true;
          os << tv.second;
          break;
        case TOK_BINOP:
          if (!saw_operand) {
            error = true;
            break;
          }
          saw_operand = false;
          expect_operand = true;
          os << tv.second;
          break;
        case TOK_OPEN_PAREN:
          saw_operand = false;
          expect_operand = false;
          os << tv.second;
          break;
        case TOK_CLOSE_PAREN:
          if (expect_operand) {
            error = true;
            break;
          }
          saw_operand = true;
          os << tv.second;
          break;
        case TOK_STRING_CONSTANT:
          if (saw_operand) {
            error = true;
            break;
          }
          saw_operand = true;
          expect_operand = false;
          os << tv.second;
          break;
        default:
          error = true;
          break;
      }
  }
  if (error || expect_operand) {
    m->mstate = VisitedWithError;
    return;
  }
  m->expanded = os.str();
  auto notEmpty = m->expanded.find_first_not_of(" \t");
  if (notEmpty == std::string::npos) {
    m->mstate = VisitedEmpty;
  } else {
    m->mstate = VisitedOK;
  }
}

void MacroParser::postProcessMacros()
{
  std::vector<MacroDef *> defs;
  for (auto it = macros_.begin(); it != macros_.end(); it++)
    defs.push_back(const_cast<MacroDef*>(&(*it)));
  for (auto it = defs.begin(); it != defs.end(); it++)
    expandMacro(*it);
}

void MacroParser::emitMacros(llvm::raw_ostream &os,
                             const std::unordered_set<std::string> &emittedTypes)
{
  // Collect all emittable macros
  std::vector<MacroDef *> defs;
  for (auto it = macros_.begin(); it != macros_.end(); it++) {
    MacroDef *def = const_cast<MacroDef*>(&(*it));

    // Weed out macros that had problems in parsing.
    if (def->mstate != VisitedOK)
      continue;

    // Types take precedence over macros
    if (emittedTypes.find(def->name) != emittedTypes.end())
      continue;

    // Don't emit empty macros
    auto nonEmpty = def->expanded.find_first_not_of(" \t");
    if (nonEmpty == std::string::npos)
      continue;

    defs.push_back(def);
  }

  // Sort by name for nicer output
  std::sort(defs.begin(), defs.end(),
            [](MacroDef *d1, MacroDef *d2) {
              return d1->name.compare(d2->name) < 0;
            });

  // Output a Go equivalent.
  for (auto it = defs.begin(); it != defs.end(); it++) {
    MacroDef *d = (*it);
    os << "const _" << d->name << " = " << d->expanded << "\n";
  }
}
