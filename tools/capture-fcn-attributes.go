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
// % go build capture-fcn-attributes.go
// % export PATH=<llvm bin dir>:$PATH
// To generate attributes for specified targets
// % ./capture-fcn-attributes -o HeaderFile.h -triples x86_64-unknown-linux-gnu
// % ./capture-fcn-attributes -o HeaderFile.h -triples x86_64-unknown-linux-gnu,aarch64-unknown-linux-gnu
// To generate attributes for all supported targets
// % ./capture-fcn-attributes -o HeaderFile.h
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
typedef unsigned long long uint64_t;
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

var supportedTriples []string = []string{
	"x86_64-unknown-linux-gnu",
	"aarch64-unknown-linux-gnu",
}

var (
	noclflag      = flag.Bool("noclean", false, "Don't clean temp dir")
	verbflag      = flag.Int("v", 0, "Verbose trace output level")
	cpuflag       = flag.String("cpu", "", "Generate for specified cpu or cpus")
	triplesflag   = flag.String("triples", "", "Select target triple(s)")
	outfileflag   = flag.String("o", "", "Output file")
	exitst        int
	defaultTriple string
)

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
	log.Fatalf(s, a...)
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

type tripleResults struct {
	results []result
	triples []string
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

// enumerateAttributesForCPU invokes clang to capture the attribute
// set for a given CPU within a given target, sending the results to
// the specified channel.
func enumerateAttributesForCPU(triple string, tdir string, cpu string, tf string, rchan chan result) {

	//
	// Confusingly, for some targets "-mcpu=XXX" is required (use of
	// -march=XXX will be rejected), whereas for other targets if you
	// use -mcpu=XXX instead of -march=XXX the compile will succeed,
	// but you'll get a warning message of the form "warning: argument
	// unused during compilation: '-mcpu=XXX'". To work around this,
	// try first -mcpu and then fall back on -march if the warning
	// is generated for -mcpu.
	//
	attempts := []string{"cpu", "arch"}
	lloutfile := filepath.Join(tdir, fmt.Sprintf("%s.ll", cpu))
	for _, cpuarch := range attempts {
		clargs := []string{"-emit-llvm", "-S", "-o", lloutfile,
			"-O3", "-Xclang", "-disable-llvm-passes", tf}
		if triple != "" {
			clargs = append(clargs, fmt.Sprintf("--target=%s", triple))
		}
		if cpu != "" {
			clargs = append(clargs, fmt.Sprintf("-m%s=%s", cpuarch, cpu))
		}
		emitClangCmdLine(tdir, cpu, clargs)
		cmd := exec.Command("clang", clargs...)
		output, cerr := cmd.CombinedOutput()
		if cerr != nil {
			if *verbflag > 0 || triple == "" {
				warn("clang run failed: %s", output)
			}
			if triple == "" {
				fatal("err = %v", cerr)
			}

			// Note the 'supported:false' (indicating that this CPU
			// value is not viable).
			rchan <- result{cpu: cpu, attrs: strings.Join(clargs, " "), supported: false, def: false}
			return
		} else {
			// Look for "argument unused" warning.
			warning := "argument unused during compilation: '-mcpu="
			if strings.Contains(string(output), warning) {
				// Move on to try -march
				continue
			}
			// Sift through the output for attr set.
			acpu, attrs := parseClangOutFile(lloutfile)
			adef := false
			if cpu == "" || cpu == "generic" && acpu == "generic" {
				adef = true
			}
			// Send results on to the consumer.
			rchan <- result{cpu: acpu, attrs: attrs, supported: true, def: adef}
			return
		}
	}
}

func enumerateAttributes(triple string, tdir string, cpus []string, tf string) []result {

	verb(1, "enumerating attributes for %d cpus", len(cpus))

	tripleTmp := filepath.Join(tdir, triple)
	mkdirErr := os.Mkdir(tripleTmp, 0755)
	if mkdirErr != nil {
		fatal("unable to make tmp dir: %v", mkdirErr)
	}

	// First entry in the list needs to be the default CPU
	ecpus := append([]string{""}, cpus...)

	// Process the various CPUs in parallel
	rchan := make(chan result, runtime.NumCPU()) // limit concurrency
	defer close(rchan)
	for _, cpu := range ecpus {
		verb(1, "enumerate for cpu %s", cpu)
		go enumerateAttributesForCPU(triple, tripleTmp, cpu, tf, rchan)
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

	// Sort
	sort.Sort(ByCpu(results))
	return results
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

func genCPUs(triple string) []string {

	// If -cpu XXX then just use that.
	if *cpuflag != "" {
		return strings.Split(*cpuflag, ",")
	}

	// Alternatively, look at the output of "llc -mcpu=help" (running
	// clang with "-march=IllegalBadVal" also has similar effects, but
	// doesn't allow you to set the triple currently).

	tgtopt := []string{"-mcpu=help", fmt.Sprintf("-mtriple=%s", triple)}
	cmd := exec.Command("llc", tgtopt...)
	output, cerr := cmd.CombinedOutput()
	if cerr != nil {
		warn("llc run failed: %s", output)
		fatal("err = %v", cerr)
	}
	verb(3, "llc output is: %s\n", string(output))

	// Parse the output
	resultcpus := []string{}
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
	// clang --version includes full output from git remote -v, which
	// can include github access token (we definitely don't want to
	// share that); strip it out if we see something like that.
	m := regexp.MustCompile(`://(\S+:\S+)@`)
	trimmed := m.ReplaceAllString(string(output), "://")
	sl := strings.Split(trimmed, "\n")
	bw.WriteString("//  ")
	bw.WriteString(sl[0])
	bw.WriteString("\n//\n")
	bw.WriteString(pream2)
}

func epilog(bw *bufio.Writer, triplesResults []tripleResults) {
	bw.WriteString("const TripleCpus triples[] = {\n")
	for k, tr := range triplesResults {
		for _, trip := range tr.triples {
			bw.WriteString(fmt.Sprintf("  { \"%s\", &attrs%d[0] },\n", trip, k))
		}
	}
	bw.WriteString("  { \"\", nullptr } // sentinel\n")
	bw.WriteString("};\n")
}

func Equal(a, b []result) bool {
	if len(a) != len(b) {
		return false
	}
	for i := 0; i < len(a); i++ {
		if a[i] != b[i] {
			return false
		}
	}
	return true
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
	triplesSpecified := strings.Split(*triplesflag, ",")
	triples := []string{}
	if len(*triplesflag) == 0 {
		triples = append(triples, supportedTriples...)
	} else {
		triples = append(triples, triplesSpecified...)
	}
	bw := bufio.NewWriter(outfile)
	prolog(bw)
	resultsList := []tripleResults{}
	for _, trip := range triples {
		// CPU selection (either from option or via clang/llc)
		cpus := genCPUs(trip)
		// Enumerate attributes for the specified CPUs
		results := enumerateAttributes(trip, dir, cpus, tf)
		exist := false
		for i := 0; i < len(resultsList); i++ {
			rl := &resultsList[i]
			if Equal(results, rl.results) {
				exist = true
				rl.triples = append(rl.triples, trip)
			}
		}
		if !exist {
			resultsList = append(resultsList, tripleResults{results, []string{trip}})
		}
	}
	for k, res := range resultsList {
		fmt.Fprintf(bw, "// triple:")
		for i := 0; i < len(res.triples); i++ {
			trip := res.triples[i]
			if i != len(res.triples)-1 {
				fmt.Fprintf(bw, " %s,", trip)
			} else {
				fmt.Fprintf(bw, " %s", trip)
			}
		}
		fmt.Fprintf(bw, "\n")
		fmt.Fprintf(bw, "static const CpuAttrs attrs%d[] = {\n", k)
		bw.WriteString("  // first entry is default cpu\n")
		for i := 0; i < len(res.results); i++ {
			r := res.results[i]
			fmt.Fprintf(bw, "  { \"%s\", \"%s\"},\n", r.cpu, r.attrs)
		}
		bw.WriteString("  { \"\", \"\" } // sentinel\n")
		bw.WriteString("};\n\n")
	}
	epilog(bw, resultsList)
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
