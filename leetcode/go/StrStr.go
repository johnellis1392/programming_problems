package main

import (
	"fmt"
)

type Test struct {
	haystack, needle string
	output           int
}

var tests = []Test{
	{"sadbutsad", "sad", 0},
	{"leetcode", "leeto", -1},
	{"a", "a", 0},
	{"ba", "a", 1},
}

func strStr(haystack, needle string) int {
	if len(needle) == 0 || len(needle) > len(haystack) {
		return -1
	}
	n, m := len(haystack), len(needle)
	for i := 0; i < n-m+1; i++ {
		if haystack[i:i+m] == needle {
			return i
		}
	}
	return -1
}

func main() {
	fmt.Println("Running...")
	for _, t := range tests {
		res := strStr(t.haystack, t.needle)
		if res == t.output {
			fmt.Println("Success")
		} else {
			fmt.Printf("Failure: %v != %v\n", res, t.output)
		}
	}
}
