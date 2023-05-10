package main

import (
	"fmt"
)

type Test struct {
	input  []int
	output int
	a      []int
}

var tests = []Test{
	{[]int{1, 2, 3}, 3, []int{1, 2, 3}},
	{[]int{1, 1, 2}, 2, []int{1, 2}},
	{[]int{0, 0, 1, 1, 1, 2, 2, 3, 3, 4}, 5, []int{0, 1, 2, 3, 4}},
}

func removeDuplicates2(nums []int) int {
	i, k := 0, 1
	for i < len(nums)-1 {
		if nums[i] == nums[i+1] {
			nums = append(nums[:i], nums[i+1:]...)
		} else {
			k++
			i++
		}
	}
	return k
}

func removeDuplicates(nums []int) int {
	i, k := 0, 1
	for i < len(nums)-1 {
		if nums[i] == nums[i+1] {
			temp := nums[i]
			for j := i; j < len(nums)-1; j++ {
				nums[j] = nums[j+1]
			}
			nums[len(nums)-1] = temp
		} else {
			i++
			k++
		}
	}
	nums = nums[:k]
	return k
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
		res := removeDuplicates(t.input)
		if res == t.output || !equals(t.input, t.a) {
			fmt.Printf("Success: a=%v\n", t.a)
		} else {
			fmt.Printf("Failure: %v != %v\n", res, t.output)
		}
	}
}
