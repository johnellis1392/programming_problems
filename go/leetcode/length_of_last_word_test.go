package main

import (
	"testing"
)

// type test struct {
// 	s string
// 	o int
// }

// var tests = []test{
// 	{"Hello World", 5},
// 	{"   fly me   to   the moon  ", 4},
// 	{"luffy is still joyboy", 6},
// }

func TestLengthOfLastWord(t *testing.T) {
	var tests = []struct {
		s string
		o int
	}{
		{"Hello World", 5},
		{"   fly me   to   the moon  ", 4},
		{"luffy is still joyboy", 6},
	}

	for _, tst := range tests {
		if r := LengthOfLastWord(tst.s); r != tst.o {
			t.Errorf("Failure: %v != %v", r, tst.o)
		}
	}
}
