package main

import (
	"fmt"
)

type Test struct {
	nums   []int
	target int
	exp    int
}

var tests = []Test{
	{[]int{1, 3, 5, 6}, 5, 2},
	{[]int{1, 3, 5, 6}, 2, 1},
	{[]int{1, 3, 5, 6}, 7, 4},
}

func searchInsert(nums []int, target int) int {
	i, j := 0, len(nums)
	for i < j {
		mid := (i + j) / 2
		if nums[mid] == target {
			return mid
		} else if nums[mid] < target {
			i = mid + 1
		} else {
			j = mid
		}
	}
	return i
}

func main() {
	fmt.Println("Running...")
	for _, t := range tests {
		res := searchInsert(t.nums, t.target)
		if res == t.exp {
			fmt.Println("Success")
		} else {
			fmt.Printf("Failure: %v != %v\n", res, t.exp)
		}
	}
}
