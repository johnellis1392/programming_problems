package main

import (
	"testing"
)

func TestRemoveDuplicates(t *testing.T) {
	var tests = []struct {
		i []int
		o int
		a []int
	}{
		{[]int{1, 2, 3}, 3, []int{1, 2, 3}},
		{[]int{1, 1, 2}, 2, []int{1, 2}},
		{[]int{0, 0, 1, 1, 1, 2, 2, 3, 3, 4}, 5, []int{0, 1, 2, 3, 4}},
	}

	equals := func(a, b []int, k int) bool {
		if len(a) < len(b) || k > len(b) {
			return false
		}
		for i := 0; i < k; i++ {
			if a[i] != b[i] {
				return false
			}
		}
		return true
	}

	for _, tst := range tests {
		if k := RemoveDuplicates(tst.i); k != tst.o || !equals(tst.i, tst.a, k) {
			t.Errorf("Failure: %v != %v, %v != %v", k, tst.o, tst.i, tst.a)
		}
	}
}
