//===-- macro-tokenizer.cpp - definitions for godumpsecp tokenizer --------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Definitions for MacroTokenizer class
//
//===----------------------------------------------------------------------===//

#include "macro-tokenizer.h"

#include <iostream>

std::pair<MacTokenTyp, std::string>
MacroTokenizer::getToken()
{
  if (done())
    return std::make_pair(TOK_END_OF_STRING, std::string());

  std::cerr << "pos: " << pos_ << " cur: " << cur() << " int " << ((int)cur()) << "\n";

  switch(cur()) {
    case ')':
      return std::make_pair(TOK_CLOSE_PAREN, consume1());
    case '(':
      return std::make_pair(TOK_OPEN_PAREN, consume1());
    case '%': case '/': case '*': case '|': case '&': case '^':
      return std::make_pair(TOK_BINOP, consume1());
    case '<':
    case '>': {
      std::string tok(consume1());
      if (cur() == tok[0] || cur() == '=')
        tok += consume1();
      return std::make_pair(TOK_BINOP, tok);
    }
    case '=': {
      std::string tok = consume1();
      if (cur() == '=') {
        tok += consume1();
        return std::make_pair(TOK_BINOP, tok);
      }
      return std::make_pair(TOK_ERROR, tok);
    }
    case '!': {
      std::string tok = consume1();
      if (cur() == '=') {
        tok += consume1();
        return std::make_pair(TOK_BINOP, tok);
      }
      // assume unary
      return std::make_pair(TOK_UNOP, tok);
    }
    case '~':
      consume1();
      return std::make_pair(TOK_UNOP, std::string("^"));
    case '-':
    case '+':
      return std::make_pair(TOK_ADDSUB, consume1());
    case ' ':
    case '\t': {
      std::string tok(consume([](char c) { return c == ' ' || c == '\t'; }));
      return std::make_pair(TOK_SPACE, tok);
    }
    case '.':
      if (!isdigit(next(1)))
        return std::make_pair(TOK_ERROR, ".");
      // fall through;
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9': {
      bool hexcon = false;
      std::string tok;
      if (cur() == '0' && (next(1) == 'x' || next(1) == 'X')) {
        hexcon = true;
        tok = consumeN(2);
      }
      tok += consume([hexcon](char c) {
          return (hexcon ? isxdigit(c) : (isdigit(c) || c == '.' ||
                                          c == 'e' || c == 'E'));
        });
      // Chop off any trailing stuff (not allowed in go)
      while (!done() &&
             (cur() == 'u' || cur() == 'U' ||
              cur() == 'f' || cur() == 'F' ||
              cur() == 'd' || cur() == 'D' ||
              cur() == 'l' || cur() == 'L'))
        consume1();
      return std::make_pair(TOK_NUMERIC_CONSTANT, tok);
    }
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
    case 'G': case 'H': case 'I': case 'J': case 'K': case 'L':
    case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
    case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
    case 'Y': case 'Z':
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
    case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
    case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
    case 's': case 't': case 'u': case 'v': case 'w': case 'x':
    case 'y': case 'z':
    case '_': {
      // Identifier
      std::string tok;
      tok = consume([](char c) { return isalnum(c) || 'c' == '_'; });
      return std::make_pair(TOK_IDENTIFIER, tok);
    }
    case '"':
    case '\'': {
      // NB: this rule is sloppy about what's permitted
      // (allows things like 'abcdef' in addition to "abcdef").
      char quote = cur();
      std::string tok;
      tok += consume1();
      tok += consume2([quote](char curc, char prevc) {
          return (curc != quote || prevc == '\\');
        });
      if (done())
        return std::make_pair(TOK_ERROR, std::string());
      tok += consume1();
      return std::make_pair(TOK_STRING_CONSTANT, tok);
    }
    default:
      break;
  }
  return std::make_pair(TOK_ERROR, std::string());
}
