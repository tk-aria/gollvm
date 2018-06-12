// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//
// This bootstrap helper program emits C++ code that encapsulates
// information about available CPUs for a given architecture. It works
// by invoking clang and/or llc and inspecting trace output and
// generated IR.   Note: the expectation is that this program will be
// built and run "off line" to generate a header file that is then
// checked in (as opposed to having it build and run as part of the
// actual gollvm ninja/cmake build).
//
// The intent is to allow gollvm to support the "-march=XXX" flag in a
// basic way without having to recreate/replicate all of the
// architecture-specific machinery in the clang driver that deals
// with feature flags and feature attributes for the available targets
// (since this code is very complex).
//
// This general idea is that for a given target triple we want to
// determine the set of legal values that can be supplied to
// the -march=XXX command line option, along with the correct set
// of feature attributes that apply for that cpu/arch (settings for
// -mattr=YYY,ZZZ,...).
//
// The strategy is to first run clang and/or LLC using command line
// options whose output (or error messages) list out available CPU
// settings (either "llc -mcpu=help" or "clang -march=Illegal"
// depending). Once the set of available CPUs is populated, we then
// run clang with -emit-llvm and inspect the generated IR to collect
// the set of attributes for each arch/cpu.
//
// Notes:
// - not all versions of clang will produce a list of legal arch/cpu
//   values when presented with an illegal -march value (this seems to be
//   a recent development); this trick also doesn't seem to work
//   when cross compiling (suppling --target=XXX to clang). For the
//   cross-compile case, we fall back on running "llc".
// - confusingly, llc's set of available CPUs is different
//   from clang's available set of CPUs, so there has to be some
//   weeding out of extra CPU names in some cases.
//
// Representative usage:
//
// % go build capture-fcn-attributes
// % export PATH=<llvm bin dir>:$PATH
// % ./capture-fcn-attributes -o HeaderFile.h -triples x86_64-unknown-linux-gnu
// %

package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"runtime"
	"sort"
	"strconv"
	"strings"
)

const prog = `
#include <inttypes.h>
typedef struct  {
  uint64_t a[512];
  uint64_t b[512];
  uint64_t c[512];
} vstuff;
void Add512(vstuff *v) {
  for (unsigned i = 0; i < 512; ++i) {
    v->c[i] = v->a[i] + v->b[i];
  }
}
`

var noclflag = flag.Bool("noclean", false, "Don't clean temp dir")
var verbflag = flag.Int("v", 0, "Verbose trace output level")
var cpuflag = flag.String("cpu", "", "Generate for specified cpu or cpus")
var triplesflag = flag.String("triples", "", "Select target triple(s)")
var outfileflag = flag.String("o", "", "Output file")
var exitst int
var defaultTriple string

func verb(vlevel int, s string, a ...interface{}) {
	if *verbflag >= vlevel {
		fmt.Printf(s, a...)
		fmt.Printf("\n")
	}
}

func warn(s string, a ...interface{}) {
	fmt.Fprintf(os.Stderr, s, a...)
	fmt.Fprintf(os.Stderr, "\n")
	exitst = 1
}

func fatal(s string, a ...interface{}) {
	log.Fatal(fmt.Sprintf(s, a...))
}

func usage(msg string) {
	if len(msg) > 0 {
		fmt.Fprintf(os.Stderr, "error: %s\n", msg)
	}
	fmt.Fprintf(os.Stderr, "usage: capture-fcn-attributes [flags]\n")
	flag.PrintDefaults()
	os.Exit(2)
}

type result struct {
	cpu       string
	attrs     string
	supported bool
	def       bool
}

func tb(x bool) int {
	if x {
		return 1
	}
	return 0
}

type ByCpu []result

func (a ByCpu) Len() int      { return len(a) }
func (a ByCpu) Swap(i, j int) { a[i], a[j] = a[j], a[i] }
func (a ByCpu) Less(i, j int) bool {
	if a[i].def != a[j].def {
		return tb(a[j].def) < tb(a[i].def)
	}
	return a[i].cpu < a[j].cpu
}

var qurx = regexp.MustCompile(`^"(.+)"$`)

func qutrim(s string) string {
	sl := qurx.FindStringSubmatch(s)
	if len(sl) == 2 {
		return string(sl[1])
	}
	return s
}

// Attributes strings are of the form { X Y Z=W A=B "Q"="R" ... }

func parseAttrs(raw string) (string, string) {
	fields := strings.Fields(raw)
	features := ""
	cpu := ""
	for _, f := range fields {
		sl := strings.Split(f, "=")
		if len(sl) != 2 {
			continue
		}
		k, v := qutrim(sl[0]), qutrim(sl[1])
		if k == "target-features" {
			features = v
		} else if k == "target-cpu" {
			cpu = v
		}
	}
	return cpu, features
}

// Function definitions in an LLVM IR dump have an attribute tag (#<num>);
// we then look for an attribute declaration with the same number later
// in the dump.

func parseClangOut(r io.Reader) (string, string) {

	// function def:
	//    define dso_local void @Add512(%struct.vstuff*) #0 {
	fcnr := regexp.MustCompile(`^define.*@Add512\(.*\)\s+#(\d)\s+{\s*$`)

	// function attrs:
	// attributes #0 = { nounwind uwtable "x"="y" .... }
	attrr := regexp.MustCompile(`^attributes\s+#(\d+)\s+\=\s+{(.+)\}\s*$`)

	rawattrs := ""
	attrnum := int64(-1)
	scanner := bufio.NewScanner(r)
	for scanner.Scan() {
		verb(3, "clangline is %s", scanner.Text())
		sl := fcnr.FindSubmatch(scanner.Bytes())
		if len(sl) == 2 {
			at, serr := strconv.ParseInt(string(sl[1]), 10, 64)
			if serr != nil {
				fatal("problems matching %s in %s",
					string(sl[1]), scanner.Text())
			}
			attrnum = at
			verb(3, "=> attrnum is %d", attrnum)
		}
		if attrnum != int64(-1) {
			sl := attrr.FindSubmatch(scanner.Bytes())
			if len(sl) == 3 {
				var at int64
				at, serr := strconv.ParseInt(string(sl[1]), 10, 64)
				if serr != nil {
					fatal("problems matching %s in %s",
						string(sl[1]), scanner.Text())
				}
				verb(3, "=-= at = %v\n", at)
				if at == attrnum {
					rawattrs = string(sl[2])
					verb(3, "=> found rawattrs %s", rawattrs)
					break
				}
			}
		}
	}
	if scanner.Err() != nil {
		fatal("error scanning clang output: %v", scanner.Err())
	}

	if rawattrs == "" {
		fatal("unable to locate fcn attrs in clang output")
	}
	return parseAttrs(rawattrs)
}

func parseClangOutFile(cloutfile string) (string, string) {
	infile, err := os.Open(cloutfile)
	if err != nil {
		fatal("problems opening clang output file %s: %s", cloutfile, err)
	}
	return parseClangOut(infile)
}

// For debugging (not needed for final output)

func emitClangCmdLine(tdir string, cpu string, clargs []string) {
	f := filepath.Join(tdir, fmt.Sprintf("%s.clangcmd.txt", cpu))
	outfile, err := os.OpenFile(f, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0666)
	if err != nil {
		fatal("unable to open trace output file %s", f)
	}
	bw := bufio.NewWriter(outfile)
	bw.WriteString("clang")
	for _, arg := range clargs {
		bw.WriteString(" ")
		bw.WriteString(arg)
	}
	bw.WriteString("\n")
	if err := bw.Flush(); err != nil {
		fatal("error writing file %s: %v", f, err)
	}
	if err := outfile.Close(); err != nil {
		fatal("error closing output file %s: %v", f, err)
	}
}

func enumerateAttributes(triple string, tdir string, cpus []string, bw *bufio.Writer, tf string, idx int) {

	verb(1, "enumerating attributes for %d cpus", len(cpus))

	// First entry in the list needs to be the default CPU
	ecpus := append([]string{""}, cpus...)

	// Process the various CPUs in parallel
	sema := make(chan struct{}, runtime.NumCPU()) // limit concurrency
	rchan := make(chan result, runtime.NumCPU())
	for _, cpu := range ecpus {
		verb(1, "enumerate for cpu %s", cpu)

		go func(cpu string) {
			sema <- struct{}{}
			defer func() {
				<-sema
			}()

			// Invoke clang with proper arguments
			lloutfile := filepath.Join(tdir, fmt.Sprintf("%s.ll", cpu))
			clargs := []string{"-emit-llvm", "-S", "-o", lloutfile,
				"-O3", "-Xclang", "-disable-llvm-passes", tf}
			cpuarch := "arch"
			if triple != "" {
				clargs = append(clargs, fmt.Sprintf("--target=%s", triple))
			}
			if triple != defaultTriple {
				cpuarch = "cpu"
			}
			if cpu != "" {
				clargs = append(clargs, fmt.Sprintf("-m%s=%s", cpuarch, cpu))
			}
			emitClangCmdLine(tdir, cpu, clargs)
			cmd := exec.Command("clang", clargs...)
			output, cerr := cmd.CombinedOutput()
			if cerr != nil {
				if triple == "" {
					warn("clang run failed: %s", output)
					fatal("err = %v", cerr)
				}
				// Note the 'supported:false' (indicating that this CPU
				// value is not viable).
				rchan <- result{cpu: cpu, attrs: strings.Join(clargs, " "), supported: false, def: false}
			} else {
				// Sift through the output for attr set.
				acpu, attrs := parseClangOutFile(lloutfile)
				adef := false
				if cpu == "" {
					adef = true
				}

				// Send results on to the consume.
				rchan <- result{cpu: acpu, attrs: attrs, supported: true, def: adef}
			}
		}(cpu)
	}

	// Read raw results.
	visited := make(map[string]bool)
	results := []result{}
	for range ecpus {
		r := <-rchan
		verb(1, "result: %v", r)
		if !r.supported {
			continue
		}
		if _, ok := visited[r.cpu]; ok {
			continue
		}
		visited[r.cpu] = true
		results = append(results, r)
	}

	// Sort, then write to output
	fmt.Fprintf(bw, "// triple: %s\n", triple)
	fmt.Fprintf(bw, "static const CpuAttrs attrs%d[] = {\n", idx)
	bw.WriteString("  // first entry is default cpu\n")
	sort.Sort(ByCpu(results))
	for i := 0; i < len(results); i++ {
		r := results[i]
		fmt.Fprintf(bw, "  { \"%s\", \"%s\" },\n", r.cpu, r.attrs)
	}
	bw.WriteString("  { \"\", \"\" } // sentinel\n")
	bw.WriteString("};\n\n")
}

// Runs llc to determine default triple value.

func collectDefaultTriple() string {
	// Run llc to collect default triple
	llcargs := []string{"--version"}
	cmd := exec.Command("llc", llcargs...)
	output, err := cmd.CombinedOutput()
	verb(3, "llc output is: %s\n", string(output))
	if err != nil {
		fatal("llc --version failed")
	}

	rx := regexp.MustCompile(`^\s*Default target:\s+(\S+)\s*$`)
	scanner := bufio.NewScanner(strings.NewReader(string(output)))
	scanner.Split(bufio.ScanLines)
	for scanner.Scan() {
		verb(3, "llc line is: %s", scanner.Text())
		asl := rx.FindSubmatch(scanner.Bytes())
		if len(asl) == 2 {
			return string(asl[1])
		}
	}

	fatal("parsing of llc --version output failed")
	return ""
}

// One way to get a list of legal CPUS: run clang with a bad
// -march setting (NB: apparently this does not work if an
// explicit target is selected).

func genCPUsVariant1(tf string) []string {

	// Invoke clang. We expect this command to fail.
	clargs := []string{"-march=IllegalBadVal", "-c", tf}
	cmd := exec.Command("clang", clargs...)
	output, _ := cmd.CombinedOutput()
	verb(3, "clang output is: %s\n", string(output))

	// Parse the output
	resultcpus := []string{""}
	cpulist := ""
	rx := regexp.MustCompile(`^note: valid target CPU values are: (\S.+)$`)
	scanner := bufio.NewScanner(strings.NewReader(string(output)))
	scanner.Split(bufio.ScanLines)
	for scanner.Scan() {
		verb(3, "clang line is: %s", scanner.Text())
		asl := rx.FindSubmatch(scanner.Bytes())
		if len(asl) == 2 {
			cpulist = string(asl[1])
			break
		}
	}
	if scanner.Err() != nil {
		fatal("error scanning clang output: %v", scanner.Err())
	}
	if cpulist == "" {
		warn("unable to find valid target CPUs in clang "+
			"output (cmd was: clang %s", strings.Join(clargs, " "))
		fatal("error output from clang: %s", output)
	}
	sl := strings.Split(cpulist, ",")
	for _, cpu := range sl {
		resultcpus = append(resultcpus, strings.Trim(cpu, " "))
	}

	return resultcpus
}

// Second way to collect CPU values for a target (run llc -mcpu=help).

func genCPUsVariant2(triple string) []string {

	// Kick off llc
	tgtopt := []string{"-mcpu=help", fmt.Sprintf("-mtriple=%s", triple)}
	cmd := exec.Command("llc", tgtopt...)
	output, cerr := cmd.CombinedOutput()
	if cerr != nil {
		warn("llc run failed: %s", output)
		fatal("err = %v", cerr)
	}
	verb(3, "llc output is: %s\n", string(output))

	// Parse the output
	resultcpus := []string{""}
	rw := regexp.MustCompile(`^\s*$`)
	r1 := regexp.MustCompile(`^Available (\S+) for this target:\s*$`)
	r2 := regexp.MustCompile(`^\s*(\S+)\s+\-\s\S.*$`)
	rtx := regexp.MustCompile(`^Use \+feature.*$`)
	rty := regexp.MustCompile(`^For example.*$`)
	scanner := bufio.NewScanner(strings.NewReader(string(output)))
	scanner.Split(bufio.ScanLines)
	which := ""
	for scanner.Scan() {
		verb(3, "llc line is: %s", scanner.Text())
		lineb := scanner.Bytes()
		if rw.Find(lineb) != nil || rtx.Find(lineb) != nil ||
			rty.Find(lineb) != nil {
			continue
		}
		asl := r1.FindSubmatch(lineb)
		if len(asl) == 2 {
			which = string(asl[1])
			continue
		}
		bsl := r2.FindSubmatch(lineb)
		if len(bsl) == 2 {
			if which == "CPUs" {
				cpu := string(bsl[1])
				resultcpus = append(resultcpus, cpu)
			}
			continue
		}
		warn("unmatched lined in llc output: %s", string(lineb))
	}
	if scanner.Err() != nil {
		fatal("error scanning llc output: %v", scanner.Err())
	}

	return resultcpus
}

func genCPUs(triple string, tf string) []string {

	// If -cpu XXX then just use that.
	if *cpuflag != "" {
		return strings.Split(*cpuflag, ",")
	}

	// Take different routes depending on whether target triple specified.
	if triple == "" {
		return genCPUsVariant1(tf)
	} else {
		return genCPUsVariant2(triple)
	}
}

const pream1 = `// DO NOT EDIT: this file auto-generated by the following command:
//
`

const pream2 = `
typedef struct {
  const char *cpu;
  const char *attrs;
} CpuAttrs;

typedef struct {
  const char *triple;
  const CpuAttrs *cpuattrs;
} TripleCpus;

`

func prolog(bw *bufio.Writer) {
	bw.WriteString(pream1)
	bw.WriteString("//   ")
	for _, arg := range os.Args {
		bw.WriteString(" ")
		bw.WriteString(arg)
	}
	bw.WriteString("\n//\n")
	bw.WriteString("// in combination with clang:\n//\n")
	cmd := exec.Command("clang", "--version")
	output, cerr := cmd.CombinedOutput()
	if cerr != nil {
		warn("clang run failed: %s", output)
		fatal("err = %v", cerr)
	}
	sl := strings.Split(string(output), "\n")
	bw.WriteString("//  ")
	bw.WriteString(sl[0])
	bw.WriteString("\n//\n")
	bw.WriteString(pream2)
}

func epilog(bw *bufio.Writer, triples []string) {
	bw.WriteString("const TripleCpus triples[] = {\n")
	for k, t := range triples {
		bw.WriteString(fmt.Sprintf("  { \"%s\", &attrs%d[0] },\n", t, k))
	}
	bw.WriteString("  { \"\", nullptr } // sentinel\n")
	bw.WriteString("};\n")
}

func perform() {

	// Create tempdir
	dir, err := ioutil.TempDir("", "CaptureFcnAttrsTempDir")
	if err != nil {
		fatal("ioutil.TempDir failed, err=%v", err)
	}
	if *noclflag {
		defer func() { fmt.Printf("preserving temp dir %s\n", dir) }()
	} else {
		defer os.RemoveAll(dir)
	}

	// Emit tempfile
	tf := filepath.Join(dir, "file.c")
	verb(1, "temp file is %s", tf)
	if err := ioutil.WriteFile(tf, []byte(prog), 0666); err != nil {
		fatal("ioutil.WriteFile failed, err=%v", err)
	}

	// Open output file
	var outfile = os.Stdout
	if len(*outfileflag) > 0 {
		verb(1, "opening %s", *outfileflag)
		outfile, err = os.OpenFile(*outfileflag,
			os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0666)
		if err != nil {
			log.Fatal(err)
		}
	}

	defaultTriple = collectDefaultTriple()
	triples := strings.Split(*triplesflag, ",")
	if len(triples) == 0 {
		triples = append(triples, defaultTriple)
	}
	bw := bufio.NewWriter(outfile)
	prolog(bw)
	for k, trip := range triples {

		// CPU selection (either from option or via clang/llc)
		cpus := genCPUs(trip, tf)

		// Enumerate attributes for the specified CPUs
		enumerateAttributes(trip, dir, cpus, bw, tf, k)
	}
	epilog(bw, triples)
	if err := bw.Flush(); err != nil {
		fatal("error writing output: %v", err)
	}
	if len(*outfileflag) > 0 {
		if err := outfile.Close(); err != nil {
			fatal("error closing output file %s: %v", *outfileflag, err)
		}
	}
}

func main() {
	log.SetFlags(0)
	log.SetPrefix("capture-fcn-attributes: ")
	flag.Parse()
	verb(1, "in main")
	if flag.NArg() != 0 {
		usage("please run without arguments")
	}
	perform()
	verb(1, "leaving main")
	os.Exit(exitst)
}
