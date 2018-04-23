//===-- Action.h ----------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
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
    A_Input,
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
      : Action(Action::A_Input),
        input_(input) { }
  Artifact *input() const { return input_; }
 private:
  Artifact *input_;
};

inline InputAction *Action::castToInputAction() {
  return type_ == A_Input ? static_cast<InputAction*>(this) : nullptr;
}
inline const InputAction *Action::castToInputAction() const {
  return type_ == A_Input ? static_cast<const InputAction*>(this) : nullptr;
}

} // end namespace driver
} // end namespace gollvm

#endif // GOLLVM_DRIVER_ACTION_H
