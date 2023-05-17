package main

import (
	"testing"
)

func TestIsPalindrome(t *testing.T) {
	var tests = []struct {
		in  int
		out bool
	}{
		{5, true},
		{121, true},
		{-121, false},
		{10, false},
	}

	for _, tst := range tests {
		actual := IsPalindrome(tst.in)
		if actual != tst.out {
			t.Errorf("Failure: %v != %v", actual, tst.out)
		}
	}
}
