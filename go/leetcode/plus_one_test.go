package main

import (
	"testing"
)

func TestPlusOne(t *testing.T) {
	equal := func(a, b []int) bool {
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

	var tests = []struct {
		i []int
		o []int
	}{
		{[]int{1, 2, 3}, []int{1, 2, 4}},
		{[]int{4, 3, 2, 1}, []int{4, 3, 2, 2}},
		{[]int{9}, []int{1, 0}},
	}

	for _, tst := range tests {
		if o := PlusOne(tst.i); !equal(o, tst.o) {
			t.Errorf("plusOne(%v) == %v, want %v", tst.i, o, tst.o)
		}
	}
}
