package main

import (
	"fmt"
)

type Test struct {
	nums []int
	val  int
	k    int
	exp  []int
}

var tests = []Test{
	{[]int{3, 2, 2, 3}, 3, 2, []int{2, 2}},
	{[]int{0, 1, 2, 2, 3, 0, 4, 2}, 2, 5, []int{0, 1, 4, 0, 3}},
}

func removeElement(nums []int, val int) int {
	k := len(nums)
	i := 0
	for i < k {
		if nums[i] == val {
			temp := nums[i]
			nums[i] = nums[k-1]
			nums[k-1] = temp
			k--
		} else {
			i++
		}
	}
	return k
}

func equals(a, b []int, k int) bool {
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

func main() {
	fmt.Println("Running...")
	for _, t := range tests {
		res := removeElement(t.nums, t.val)
		if res == t.k && equals(t.nums, t.exp, t.k) {
			fmt.Println("Success")
		} else {
			fmt.Printf(
				"Failure: %d != %d, %v != %v\n",
				res, t.k, t.nums, t.exp,
			)
		}
	}
}
