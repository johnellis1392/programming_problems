package main

import (
	"fmt"
	"math"
	"strconv"
)

func isPalindrome2(x int) bool {
	if x < 0 {
		return false
	} else if x == 0 {
		return true
	}
	n := int(math.Log10(float64(x))) + 1
	v := make([]int, n)
	for i := 0; i < n; i++ {
		v[i] = x % 10
		x /= 10
	}
	for i := 0; i < len(v)/2; i++ {
		if v[i] != v[len(v)-i-1] {
			return false
		}
	}
	return true
}

func isPalindrome(x int) bool {
	if x < 0 {
		return false
	}
	s := strconv.FormatInt(int64(x), 10)
	n := len(s)
	for i := 0; i < n/2; i++ {
		if s[i] != s[n-i-1] {
			return false
		}
	}
	return true
}

type test struct {
	in  int
	out bool
}

var tests = []test{
	{5, true},
	{121, true},
	{-121, false},
	{10, false},
}

func main() {
	fmt.Println("Running...")
	for _, t := range tests {
		actual := isPalindrome(t.in)
		if actual == t.out {
			fmt.Println("Success")
		} else {
			fmt.Printf("Failure: %v != %v\n", actual, t.out)
		}
	}
}
