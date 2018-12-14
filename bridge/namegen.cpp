//===-- namegen.cpp -------------------------------------------------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//

// Implementation of NameGen helper class.

#include "namegen.h"

  // For creating useful type, inst and block names.
std::string NameGen::namegen(const std::string &tag,
                             unsigned expl)
{
  auto it = nametags_.find(tag);
  unsigned count = 0;
  if (it != nametags_.end())
    count = it->second + 1;
  if (expl != ChooseVer)
    count = expl;
  std::stringstream ss;
  ss << tag << "." << count;
  if (expl == ChooseVer)
    nametags_[tag] = count;
  return ss.str();
}

static std::string chopNumericSuffix(const std::string &tag)
{
  std::string res;
  auto dot = tag.rfind('.');
  if (dot == std::string::npos || dot == tag.size()-1)
    return tag;
  dot += 1;
  if (!std::isdigit(tag.at(dot)))
    return tag;
  auto found = tag.find_first_not_of("0123456789", dot);
  if (found != std::string::npos)
    return tag;
  return tag.substr(0, dot-1);
}

std::string NameGen::combineTags(const std::string &baseTag,
                                 const std::string &suffix)
{
  std::string res(chopNumericSuffix(baseTag));
  res += suffix;
  return res;
}
