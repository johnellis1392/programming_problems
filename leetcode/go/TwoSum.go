package main

import (
	"fmt"
)

type test struct {
	nums   []int
	target int
	exp    []int
}

func twoSum(nums []int, target int) []int {
	m := make(map[int]int)
	for i, v := range nums {
		if j, ok := m[v]; ok {
			return []int{j, i}
		} else {
			m[target-v] = i
		}
	}
	return []int{}
}

var tests = []test{
	{[]int{2, 7, 11, 15}, 9, []int{0, 1}},
	{[]int{3, 2, 4}, 6, []int{1, 2}},
	{[]int{3, 3}, 6, []int{0, 1}},
}

func equals(a, b []int) bool {
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

func main() {
	fmt.Println("Running...")
	for _, t := range tests {
		actual := twoSum(t.nums, t.target)
		if equals(actual, t.exp) {
			fmt.Println("Success")
		} else {
			fmt.Printf("Failure: %v != %v\n", actual, t.exp)
		}
	}
}
