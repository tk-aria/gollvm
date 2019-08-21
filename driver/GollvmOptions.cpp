//===--- GollvmOptions.cpp - Gollvm Driver Options Table ------------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//

#include "GollvmOptions.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Option/Option.h"

using namespace llvm::opt;

namespace gollvm {
namespace options {

#define PREFIX(NAME, VALUE) static const char *const NAME[] = VALUE;
#include "GollvmOptions.inc"
#undef PREFIX

static const OptTable::Info InfoTable[] = {
#define OPTION(PREFIX, NAME, ID, KIND, GROUP, ALIAS, ALIASARGS, FLAGS, PARAM,  \
               HELPTEXT, METAVAR, VALUES)                                      \
  {PREFIX, NAME,  HELPTEXT,    METAVAR,     OPT_##ID,  Option::KIND##Class,    \
   PARAM,  FLAGS, OPT_##GROUP, OPT_##ALIAS, ALIASARGS, VALUES},
#include "GollvmOptions.inc"
#undef OPTION
};

namespace {

class DriverOptTable : public OptTable {
public:
  DriverOptTable()
      : OptTable(InfoTable) {}
};

}

std::unique_ptr<OptTable> createGollvmDriverOptTable() {
  auto Result = std::make_unique<DriverOptTable>();
  return std::move(Result);
}

}
}
