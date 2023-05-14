package main

import (
	"fmt"
)

type Test struct {
	i []int
	o []int
}

var tests = []Test{
	{[]int{1, 2, 3}, []int{1, 2, 4}},
	{[]int{4, 3, 2, 1}, []int{4, 3, 2, 2}},
	{[]int{9}, []int{1, 0}},
}

func PlusOne(digits []int) []int {
	for i := len(digits) - 1; i >= 0; i-- {
		if digits[i] < 9 {
			digits[i]++
			break
		} else {
			digits[i] = 0
		}
	}
	if digits[0] == 0 {
		digits = append(digits, 0)
		for i := len(digits) - 1; i > 0; i-- {
			digits[i] = digits[i-1]
		}
		digits[0] = 1
	}
	return digits
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
		r := PlusOne(t.i)
		if equals(r, t.o) {
			fmt.Println("Success")
		} else {
			fmt.Printf("Failure: %v == %v\n", r, t.o)
		}
	}
}
