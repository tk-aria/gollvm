//===-- go-llvm-containertypes.h - hashmap/hashset type declarations ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Templated type declarations for hash map/set helper classes.
//
//===----------------------------------------------------------------------===//

#ifndef LLVMGOFRONTEND_GO_LLVM_CONTAINERTYPES_H
#define LLVMGOFRONTEND_GO_LLVM_CONTAINERTYPES_H

#include <unordered_map>
#include <unordered_set>

// In various bridge classes it is useful to keep tables that incorporate
// a key of pair type <X,Y> and with some arbitrary value type. Set up a
// templatized class 'pairvalmap' to help with this.

template <typename T1, typename T2> struct pairvalmap_hash {
  typedef std::pair<T1, T2> pairtype;
  unsigned int operator()(const pairtype &p) const {
    std::size_t h1 = std::hash<T1>{}(p.first);
    std::size_t h2 = std::hash<T2>{}(p.second);
    return h1 + h2;
  }
};

template <typename T1, typename T2> struct pairvalmap_equal {
  typedef std::pair<T1, T2> pairtype;
  bool operator()(const pairtype &p1, const pairtype &p2) const {
    return (p1.first == p2.first && p1.second == p2.second);
  }
};

template <typename T1, typename T2, typename V>
using pairvalmap =
    std::unordered_map<std::pair<T1, T2>, V, pairvalmap_hash<T1, T2>,
                       pairvalmap_equal<T1, T2>>;

template <typename T1, typename T2>
using pairhashset =
    std::unordered_set<std::pair<T1, T2>, pairvalmap_hash<T1, T2>,
                       pairvalmap_equal<T1, T2>>;

#endif // LLVMGOFRONTEND_GO_LLVM_CONTAINERTYPES_H
