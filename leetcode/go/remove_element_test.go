package main

import (
	"testing"
)

func TestRemoveElement(t *testing.T) {
	var tests = []struct {
		nums []int
		val  int
		k    int
		exp  []int
	}{
		{[]int{3, 2, 2, 3}, 3, 2, []int{2, 2}},
		{[]int{0, 1, 2, 2, 3, 0, 4, 2}, 2, 5, []int{0, 1, 4, 0, 3}},
	}

	equals := func(a, b []int, k int) bool {
		if len(a) < k || len(b) < k {
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
		if res := RemoveElement(tst.nums, tst.val); res != tst.k || !equals(tst.nums, tst.exp, tst.k) {
			t.Errorf("Failure: %d != %d, %v != %v", res, tst.k, tst.nums, tst.exp)
		}
	}
}
