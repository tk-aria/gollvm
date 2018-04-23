//===-- Tool.cpp ----------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Gollvm driver helper class Tool methods.
//
//===----------------------------------------------------------------------===//

#include "Tool.h"

namespace gollvm {
namespace driver {

Tool::Tool(const char *name, ToolChain &tc, ToolClass klass)
    : name_(name), toolchain_(tc), klass_(klass)
{
}

Tool::~Tool()
{
}

InternalTool::InternalTool(const char *name, ToolChain &tc)
      : Tool(name, tc, Tool::Internal)
{
}

InternalTool::~InternalTool()
{
}

ExternalTool::ExternalTool(const char *name, ToolChain &tc)
      : Tool(name, tc, Tool::External)
{
}

ExternalTool::~ExternalTool()
{
}

} // end namespace driver
} // end namespace gollvm
