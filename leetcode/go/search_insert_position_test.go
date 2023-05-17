package main

import (
	"testing"
)

func TestSearchInsert(t *testing.T) {
	var tests = []struct {
		nums   []int
		target int
		exp    int
	}{
		{[]int{1, 3, 5, 6}, 5, 2},
		{[]int{1, 3, 5, 6}, 2, 1},
		{[]int{1, 3, 5, 6}, 7, 4},
	}

	for _, tst := range tests {
		if res := SearchInsert(tst.nums, tst.target); res != tst.exp {
			t.Errorf("Failure: %v != %v", res, tst.exp)
		}
	}
}
