// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Rudimentary program for remastering unit test expected outputs,
// typically useful in situations where there are large numbers
// of outputs that have changed as a result of a difference in
// the LLVM IR dumper. Usage:
//
// $ GOLLVM_UNITTESTS_EMIT_REMASTER_SCRIPT=1 \
//     ./tools/gollvm/unittests/BackendCore/GoBackendCoreTests
// <test output>
// ... emitting remaster inputs to file '/tmp/remaster-inputs.txt'
// $ go build remaster-tests.go
// $ ./remaster-tests /tmp/remaster-inputs.txt
//
// WARNING WARNING WARNING: this program overwrites the unit test
// source files in place, so it should be used with caution.
//

package main

import (
	"bufio"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"
)

var verbflag = flag.Int("v", 0, "Verbose trace output level")
var dryrunflag = flag.Bool("dryrun", false, "Show actions but don't overwrite sources.")

func verb(vlevel int, s string, a ...interface{}) {
	if *verbflag >= vlevel {
		fmt.Printf(s, a...)
		fmt.Printf("\n")
	}
}

func usage(msg string) {
	if len(msg) > 0 {
		fmt.Fprintf(os.Stderr, "error: %s\n", msg)
	}
	fmt.Fprintf(os.Stderr, "usage: remaster-tests [flags] <inputfile>\n")
	flag.PrintDefaults()
	os.Exit(2)
}

type Change struct {
	expected string
	actual   string
	line     int
	mls      bool // macro line at start (vs end)
}

type FileChanges struct {
	changes []Change
}

// readInput reads in the /tmp/remaster-inputs.txt file and inserts
// the changes it describes into a handy map.
func readInput(inf *os.File) map[string]FileChanges {
	fm := make(map[string]FileChanges)
	scanner := bufio.NewScanner(inf)
	lno := 1
	for scanner.Scan() {
		line := scanner.Text()
		verb(3, "=-= line is %s", line)
		tokens := strings.Split(line, " ")
		if len(tokens) != 5 {
			panic(fmt.Sprintf("malformed line %d in input file: %s", lno, line))
		}
		macroLineAtStart, errm := strconv.ParseInt(tokens[0], 10, 64)
		if errm != nil {
			panic(fmt.Sprintf("malformed srcline in input line %d: %s", lno, line))
		}
		mls := false
		if macroLineAtStart == 1 {
			mls = true
		}
		srcfile := tokens[1]
		spot, err := strconv.ParseInt(tokens[2], 10, 64)
		if err != nil {
			panic(fmt.Sprintf("malformed srcline in input line %d: %s", lno, line))
		}
		exp := tokens[3]
		act := tokens[4]
		fc := fm[srcfile]
		newchange := Change{
			expected: exp,
			actual:   act,
			line:     int(spot),
			mls:      mls,
		}
		fc.changes = append(fc.changes, newchange)
		fm[srcfile] = fc
	}
	return fm
}

func filesHaveSameContent(f1 string, f2 string) bool {
	content1, err1 := ioutil.ReadFile(f1)
	if err1 != nil {
		log.Fatal(err1)
	}
	content2, err2 := ioutil.ReadFile(f2)
	if err2 != nil {
		log.Fatal(err2)
	}
	return string(content1) == string(content2)
}

func isWhite(s string) bool {
	return strings.TrimSpace(s) == ""
}

func readFile(fn string) []string {
	// Read
	inf, err := os.Open(fn)
	if err != nil {
		log.Fatalf("error: opening %s: %v\n", fn, err)
	}
	contents := []string{}
	scanner := bufio.NewScanner(inf)
	for scanner.Scan() {
		contents = append(contents, scanner.Text())
	}
	return contents
}

// readAndNormalizeDump reads in an expected or actual dump and
// normalizes it by removing any leading/trailing space, and
// applying a consistent level of indentation.
func readAndNormalizeDump(fn string) []string {
	contents := readFile(fn)

	// Chop off leading and trailing whitespace lines.
	var st int
	var en int
	for st = 0; st < len(contents); st++ {
		if !isWhite(contents[st]) {
			break
		}
	}
	for en = len(contents) - 1; en != 0; en-- {
		if !isWhite(contents[en]) {
			break
		}
	}
	contents = contents[st : en+1]

	// Indent.
	for i := 0; i < len(contents); i++ {
		sp := "  "
		if i == 0 {
			sp = "    "
		}
		contents[i] = sp + contents[i]
	}
	return contents
}

// applyChange applies a single remastering change to the specified
// source file. Since previous remasterings may have added or removed
// lines previously in the file, it takes a delta param to account
// for any previous additions/subtractions prior to the change
// at this line.
func applyChange(srcfile string, change Change, delta int) int {

	// Read actual and expected dumps.
	ca := readAndNormalizeDump(change.actual)
	ce := readAndNormalizeDump(change.expected)

	// Read source file and create an updated version of it.
	srclines := readFile(srcfile)
	verb(2, "%s has %d lines", srcfile, len(srclines))
	newlines := []string{}

	if *dryrunflag {
		fmt.Fprintf(os.Stderr, "dry run: applyChange stubbed out for change to %s line %d\n", srcfile, change.line)
		return 0
	}

	// Add content before the result.
	var startres int
	if change.mls {
		startres = change.line + delta
	} else {
		startres = change.line + delta - len(ca) - 1
	}
	verb(2, "startres = %d", startres)
	newlines = append(newlines, srclines[0:startres]...)

	// Add new test result.
	newlines = append(newlines, ca...)

	// Add file content after the result.
	after := startres + len(ce)
	newlines = append(newlines, srclines[after:]...)

	// Emit updated file

	f, err := os.OpenFile(srcfile, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0644)
	if err != nil {
		log.Fatal(err)
	}
	for _, s := range newlines {
		if _, err := f.WriteString(s + "\n"); err != nil {
			log.Fatalf("error writing update to %s: %v", srcfile, err)
		}
	}
	if err := f.Close(); err != nil {
		log.Fatal(err)
	}

	// Return updated delta.
	delta += len(ca) - len(ce)
	verb(2, "finished change at line %d, delta=%d", change.line, delta)
	return delta
}

// fixupFile applies all of the changes in the input that apply
// to the specified source file.
func fixupFile(file string, fc FileChanges) {
	// Sort changes by increasing line number.
	sort.SliceStable(fc.changes, func(i, j int) bool {
		ci := fc.changes[i]
		cj := fc.changes[j]
		if ci.line != cj.line {
			return ci.line < cj.line
		}
		return ci.actual < cj.actual
	})

	// Process each change. Some unit tests run once per calling
	// convention, meaning that we may encounter multiple failures
	// each at the same source line; handle this by verifying that the
	// new outputs are the same.
	delta := 0
	for k := 0; k < len(fc.changes); k++ {
		for nxt := k + 1; nxt < len(fc.changes); nxt++ {
			if fc.changes[nxt].line == fc.changes[k].line {
				// Check that actual outputs match
				if !filesHaveSameContent(fc.changes[nxt].actual,
					fc.changes[k].actual) {
					log.Fatalf("error: multiple non-identical unit test actual results found, file=%s, line=%d, dumps: %s %s\n", file, fc.changes[k].line, fc.changes[nxt].actual, fc.changes[k].actual)
				}
				k = nxt
			}
		}
		change := fc.changes[k]

		// Apply the change.
		verb(1, "change: file=%s line=%d act=%s exp=%s\n",
			file, change.line, change.actual, change.expected)
		delta = applyChange(file, change, delta)
	}
}

// perform reads in the "remaster inputs" file, builds up
// a table of updates to perform, and then applies the updates.
func perform(inf *os.File) {
	// Slurp in the input
	fm := readInput(inf)

	// Build up a list of srcfiles, then sort it
	srcs := []string{}
	for k := range fm {
		srcs = append(srcs, k)
	}
	sort.Strings(srcs)
	if len(srcs) == 0 {
		verb(0, "warning: no changes found in input file")
	}

	// Process updates for each source file.
	for _, s := range srcs {
		verb(1, "fixup for %s", s)
		fixupFile(s, fm[s])
	}
}

func main() {
	log.SetFlags(0)
	log.SetPrefix("remaster-tests: ")
	flag.Parse()
	verb(1, "entering main")
	if flag.NArg() != 1 {
		usage("please supply a single argument (file with remaster inputs)")
	}
	args := flag.Args()
	inputfile := args[0]
	f, err := os.Open(inputfile)
	if err != nil {
		log.Fatalf("error: opening %s: %v\n", inputfile, err)
	}
	defer f.Close()
	perform(f)
	verb(1, "leaving main")
}
