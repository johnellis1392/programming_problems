package main

import (
	"testing"
)

func TestTwoSum(t *testing.T) {
	var tests = []struct {
		nums   []int
		target int
		exp    []int
	}{
		{[]int{2, 7, 11, 15}, 9, []int{0, 1}},
		{[]int{3, 2, 4}, 6, []int{1, 2}},
		{[]int{3, 3}, 6, []int{0, 1}},
	}

	equals := func(a, b []int) bool {
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

	for _, tst := range tests {
		if res := TwoSum(tst.nums, tst.target); !equals(res, tst.exp) {
			t.Errorf("TwoSum(%v, %d) = %v, want %v", tst.nums, tst.target, res, tst.exp)
		}
	}
}
