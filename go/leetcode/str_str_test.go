package main

import (
	"testing"
)

func TestStrStr(t *testing.T) {
	var tests = []struct {
		haystack, needle string
		output           int
	}{
		{"sadbutsad", "sad", 0},
		{"leetcode", "leeto", -1},
		{"a", "a", 0},
		{"ba", "a", 1},
	}

	for _, tst := range tests {
		if res := StrStr(tst.haystack, tst.needle); res != tst.output {
			t.Errorf("Failure: %v != %v", res, tst.output)
		}
	}
}
