#!/usr/bin/python
"""Wrapper to selectively run gollvm instead of gccgo.

This is a shim script that intercepts invocations of 'gccgo' and then
in turn invokes either the real gccgo driver or a copy of gollvm
instead, depending on the arguments and on environment variables.

When performing a Go build with gccgo, the Go command will typically
invoke gccgo once for each compilation step, which might look like

  gccgo -I ... -o objfile.o -g <options> file.go file2.go ... fileN.go

and then a final invocation will be made at the link step, e.g.

  gccgo -L ... somearchive.a ... -o binary

The goal of this shim is to convert invocations of the first form to
llvm-goc invocations, and to ignore invocations of the second form
and just pass them on to gccgo.

We also tack on a set of additional "-L" options to the llvm-goc
invocation so that it can find the go runtime libraries, and intercept
the "-o" option so that we can run the asembler afterwards.

To use this script, you will need a copy of GCCGO, e.g. the directory
produced by running "make all && make install" in a GCCGO build tree.
From within the gccgo install dir, run

   gollvm-wrap.py --install

This will modify the install directory to insert the wrapper into the
compilation path.

"""

import getopt
import os
import re
import subprocess
import sys

import script_utils as u

# Echo command before executing
flag_echo = True

# Dry run mode
flag_dryrun = False

# gccgo only mode
flag_nollvm = False

# trace llvm-goc invocations
flag_trace_llinvoc = False


def docmd(cmd):
  """Execute a command."""
  if flag_echo:
    sys.stderr.write("executing: " + cmd + "\n")
  if flag_dryrun:
    return
  u.docmd(cmd)


def form_golibargs(driver):
  """Form correct go library args."""
  ddir = os.path.dirname(driver)
  bdir = os.path.dirname(ddir)
  cmd = "find %s/lib64 -name runtime.gox -print" % bdir
  lines = u.docmdlines(cmd)
  if not lines:
    u.error("no output from %s -- bad gccgo install dir?" % cmd)
  line = lines[0]
  rdir = os.path.dirname(line)
  u.verbose(1, "libdir is %s" % rdir)
  return rdir


def perform():
  """Main driver routine."""
  global flag_trace_llinvoc

  u.verbose(1, "argv: %s" % " ".join(sys.argv))

  # llvm-goc should be available somewhere in PATH, error if not
  lines = u.docmdlines("which llvm-goc", True)
  if not lines:
    u.error("no 'llvm-goc' in PATH -- can't proceed")

  # Perform a walk of the command line arguments looking for Go files.
  reg = re.compile(r"^\S+\.go$")
  gofile = None
  for clarg in sys.argv[1:]:
    m = reg.match(clarg)
    if m:
      gofile = clarg
      break

  if not gofile or flag_nollvm:
    # No go files. Invoke real gccgo.
    bd = os.path.dirname(sys.argv[0])
    driver = "%s/gccgo.real" % bd
    u.verbose(1, "driver path is %s" % driver)
    args = [sys.argv[0]] + sys.argv[1:]
    u.verbose(1, "args: '%s'" % " ".join(args))
    if not os.path.exists(driver):
      usage("internal error: %s does not exist [most likely this "
            "script was not installed correctly]" % driver)
    os.execv(driver, args)
    u.error("exec failed: %s" % driver)

  # Create a set of massaged args.
  nargs = []
  skipc = 0
  outfile = None
  asmfile = None
  minus_s = False
  minus_c = False
  ofiles = []
  ldflags = []
  for ii in range(1, len(sys.argv)):
    clarg = sys.argv[ii]
    if skipc != 0:
      skipc -= 1
      continue
    if clarg == "-S":
      minus_s = True
    if clarg == "-c":
      minus_c = True
    if clarg == "-o":
      outfile = sys.argv[ii+1]
      skipc = 1
      continue
    if clarg == "-v":
      flag_trace_llinvoc = True

    # redirect some gcc flags to the ones gollvm uses
    if clarg == "-O":
      clarg = "-O1"
    if clarg == "-w":
      clarg = "-no-warn"

    # dummy flags that are not supported by gollvm
    if clarg == "-lm":
      continue # TODO: if the linker is invoked, pass this to the linker?
    if clarg == "-fbounds-check":
      continue
    if clarg == "-finline-functions":
      continue
    if clarg == "-fno-diagnostics-show-caret":
      continue
    if clarg == "-fno-toplevel-reorder":
      continue
    if clarg == "-fno-var-tracking-assignments":
      continue
    if clarg == "-fomit-frame-pointer":
      continue
    if clarg == "-funroll-loops":
      continue
    if clarg == "-funsafe-math-optimizations":
      continue
    if clarg == "-gno-record-gcc-switches":
      continue
    if clarg == "-mfancy-math-387":
      continue
    if clarg == "-minline-all-stringops":
      continue
    if clarg == "-pedantic-errors":
      continue
    if clarg.startswith("-fdiagnostics-color"):
      continue
    if clarg.startswith("-fdebug-prefix-map"):
      continue

    # skip .o and .a files in compilation, but record
    # them for linker invocation.
    if clarg.endswith(".o"):
      ofiles.append(clarg)
      continue
    if clarg.endswith(".a"):
      ofiles.append(clarg)
      continue

    if clarg == "-static":
      ldflags.append(clarg)
      continue
    if clarg == "-static-libgo":
      ldflags.append(clarg)
      continue
    if clarg == "-static-libstdc++":
      ldflags.append(clarg)
      continue
    if clarg == "-static-libgcc":
      ldflags.append(clarg)
      continue

    nargs.append(clarg)

  if not outfile:
    if not minus_s and not minus_c:
      outfile = "a.out"

  if outfile:
    nargs.append("-o")
    nargs.append(outfile)

  nargs.append("-L")
  nargs.append(form_golibargs(sys.argv[0]))
  u.verbose(1, "revised args: %s" % " ".join(nargs))

  # Invoke gollvm.
  driver = "llvm-goc"
  u.verbose(1, "driver path is %s" % driver)
  nargs = ["llvm-goc"] + nargs
  if flag_trace_llinvoc:
    u.verbose(0, "+ %s" % " ".join(nargs))
  rc = subprocess.call(nargs)
  if rc != 0:
    u.verbose(1, "return code %d from %s" % (rc, " ".join(nargs)))
    return 1

    # Invoke the linker
    # Right now we use the real gccgo as the linker
    if not minus_c and not minus_s:
      bd = os.path.dirname(sys.argv[0])
      driver = "%s/gccgo.real" % bd
      ldflags += ["-o", outfile]
      ldcmd = "%s %s %s " % (driver, " ".join(ldflags), objfile)
      ldcmd += " ".join(ofiles) # pass extra .o files to the linker
      u.verbose(1, "link command is: %s" % ldcmd)
      rc = u.docmdnf(ldcmd)
      if rc != 0:
        u.verbose(1, "return code %d from %s" % (rc, ldcmd))
        return 1

  return 0


def install_shim(scriptpath):
  """Install shim into gccgo install dir."""

  # Make sure we're in the right place (gccgo install dir)
  if not os.path.exists("bin"):
    usage("expected to find bin subdir")
  if not os.path.exists("lib64/libgo.so"):
    usage("expected to find lib64/libgo.so")
  if not os.path.exists("bin/gccgo"):
    usage("expected to find bin/gccgo")

  # Copy script, or update if already in place.
  docmd("cp %s bin" % scriptpath)
  sdir = os.path.dirname(scriptpath)
  docmd("cp %s/script_utils.py bin" % sdir)

  # Test to see if script installed already
  cmd = "file bin/gccgo"
  lines = u.docmdlines(cmd)
  if not lines:
    u.error("no output from %s -- bad gccgo install dir?" % cmd)
  else:
    reg = re.compile(r"^.+ ELF .+$")
    m = reg.match(lines[0])
    if not m:
      u.warning("wrapper appears to be installed already in this dir")
      return

  # Move aside the real gccgo binary
  docmd("mv bin/gccgo bin/gccgo.real")

  # Emit a script into gccgo
  sys.stderr.write("emitting wrapper script into bin/gccgo\n")
  if not flag_dryrun:
    try:
      with open("./bin/gccgo", "w") as wf:
        here = os.getcwd()
        wf.write("#!/bin/sh\n")
        wf.write("P=%s/bin/gollvm-wrap.py\n" % here)
        wf.write("exec python ${P} \"$@\"\n")
    except IOError:
      u.error("open/write failed for bin/gccgo wrapper")
  docmd("chmod 0755 bin/gccgo")

  # Success
  u.verbose(0, "wrapper installed successfully")

  # Done
  return 0


def usage(msgarg):
  """Print usage and exit."""
  if msgarg:
    sys.stderr.write("error: %s\n" % msgarg)
  print """\
    usage:  %s <gccgo args>

    Options (via command line)
    --install   installs wrapper into gccgo directory

    Options (via GOLLVM_WRAP_OPTIONS):
    -t          trace llvm-goc executions
    -d          increase debug msg verbosity level
    -e          show commands being invoked
    -D          dry run (echo cmds but do not execute)
    -G          pure gccgo compile (no llvm-goc invocations)

    """ % os.path.basename(sys.argv[0])
  sys.exit(1)


def parse_env_options():
  """Option parsing from env var."""
  global flag_echo, flag_dryrun, flag_nollvm, flag_trace_llinvoc

  optstr = os.getenv("GOLLVM_WRAP_OPTIONS")
  if not optstr:
    return
  args = optstr.split()

  try:
    optlist, _ = getopt.getopt(args, "detDG")
  except getopt.GetoptError as err:
    # unrecognized option
    usage(str(err))

  for opt, _ in optlist:
    if opt == "-d":
      u.increment_verbosity()
    elif opt == "-e":
      flag_echo = True
    elif opt == "-t":
      flag_trace_llinvoc = True
    elif opt == "-D":
      flag_dryrun = True
    elif opt == "-G":
      flag_nollvm = True
  u.verbose(1, "env var options parsing complete")


# Setup
u.setdeflanglocale()
parse_env_options()

# Either --install mode or regular mode
if len(sys.argv) == 2 and sys.argv[1] == "--install":
  prc = install_shim(sys.argv[0])
else:
  prc = perform()
sys.exit(prc)
