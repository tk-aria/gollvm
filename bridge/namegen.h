//===-- namegen.h - decls for 'NameGen' class --======================//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Defines NameGen class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVMGOFRONTEND_NAMEGEN_H
#define LLVMGOFRONTEND_NAMEGEN_H

#include <string>
#include <sstream>
#include <unordered_map>

class NameGen {
 public:
  NameGen() { }

  // Tells namegen to choose its own version number for the created name
  static constexpr unsigned ChooseVer = 0xffffffff;

  // For creating useful type, inst and block names.
  std::string namegen(const std::string &tag, unsigned expl = ChooseVer);

  // Form a new tag name based on an existing tag and a suffix. If
  // the existing tag is versioned, strip off the version.
  static std::string combineTags(const std::string &baseTag,
                                 const std::string &suffix);

  NameGen *nameTags() {
    return const_cast<NameGen*>(this);
  }

 private:
  // Key is tag (ex: "add") and val is counter to uniquify.
  std::unordered_map<std::string, unsigned> nametags_;
};

#endif // LLVMGOFRONTEND_TYPEMANAGER_H
