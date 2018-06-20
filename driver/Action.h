//===-- Action.h ----------------------------------------------------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Defines the Action class (helper for driver functionality).
//
//===----------------------------------------------------------------------===//

#ifndef GOLLVM_DRIVER_ACTION_H
#define GOLLVM_DRIVER_ACTION_H

#include "llvm/ADT/SmallVector.h"

namespace gollvm {
namespace driver {

class Action;
class Artifact;
class InputAction;
class ReadStdinAction;

// Action lists contain pointers to actions owned by a Compilation.
typedef llvm::SmallVector<Action*, 3> ActionList;

// An abstract compilation action, such as "assemble this asm src
// file" or "link this set of objects together into a new file".
// Actions are assumed to have a single output. There is also a
// pseudo-action InputAction that that corresponds to the action of
// reading an file. An action consumes inputs from a set of other
// actions.

class Action {
 public:

  // Type of action
  enum Type {
    A_InputFile,
    A_ReadStdin,
    A_Compile,
    A_Assemble,
    A_Link
  };

  explicit Action(Type t) : type_(t) { }
  Action(Type t, Action *input) : type_(t), inputs_(ActionList({input})){ }
  Action(Type t, ActionList &il) : type_(t), inputs_(il) { }
  virtual ~Action() = default;

  const ActionList &inputs() const { return inputs_; }

  Type type() const { return type_; }

  inline InputAction *castToInputAction();
  inline const InputAction *castToInputAction() const;
  inline ReadStdinAction *castToReadStdinAction();
  inline const ReadStdinAction *castToReadStdinAction() const;

  const char *getName() const;
  const char *resultFileSuffix() const;

  // debugging
  void dump();

 private:
  Type type_;
  ActionList inputs_;
};

// An input action corresponds to the reading of some input artifact.

class InputAction : public Action {
 public:
  explicit InputAction(Artifact *input)
      : Action(Action::A_InputFile),
        input_(input) { }
  Artifact *input() const { return input_; }
 private:
  Artifact *input_;
};

// An input action that corresponds to the reading stdin.

class ReadStdinAction : public Action {
 public:
  explicit ReadStdinAction(const char *suffix)
      : Action(Action::A_ReadStdin),
        suffix_(suffix) { }
  const char *suffix() const { return suffix_; }
 private:
  const char *suffix_;
};

inline InputAction *Action::castToInputAction()
{
  return type_ == A_InputFile ? static_cast<InputAction*>(this) : nullptr;
}

inline const InputAction *Action::castToInputAction() const
{
  return type_ == A_InputFile ? static_cast<const InputAction*>(this) : nullptr;
}

inline ReadStdinAction *Action::castToReadStdinAction()
{
  return type_ == A_ReadStdin ? static_cast<ReadStdinAction*>(this) : nullptr;
}

inline const ReadStdinAction *Action::castToReadStdinAction() const
{
  return type_ == A_ReadStdin ? static_cast<const ReadStdinAction*>(this) : nullptr;
}

} // end namespace driver
} // end namespace gollvm

#endif // GOLLVM_DRIVER_ACTION_H
