package main

import (
	"testing"
)

func TestIsValid(t *testing.T) {
	var tests = []struct {
		input  string
		output bool
	}{
		{"()", true},
		{"()[]{}", true},
		{"(]", false},
		{"]", false},
	}

	for _, tst := range tests {
		if res := IsValid(tst.input); res != tst.output {
			t.Errorf("Failure: %v != %v", res, tst.output)
		}
	}
}
