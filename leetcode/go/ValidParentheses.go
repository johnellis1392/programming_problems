package main

import (
	"fmt"
)

type Test struct {
	input  string
	output bool
}

var tests = []Test{
	{"()", true},
	{"()[]{}", true},
	{"(]", false},
	{"]", false},
}

func isValid(s string) bool {
	var stack []rune
	for _, c := range s {
		if c == '(' || c == '[' || c == '{' {
			stack = append(stack, c)
		} else if len(stack) == 0 {
			return false
		} else if (c == ')' && stack[len(stack)-1] == '(') ||
			(c == ']' && stack[len(stack)-1] == '[') ||
			(c == '}' && stack[len(stack)-1] == '{') {
			stack = stack[:len(stack)-1]
		} else {
			return false
		}
	}
	return len(stack) == 0
}

func main() {
	fmt.Println("Running...")
	for _, t := range tests {
		res := isValid(t.input)
		if res == t.output {
			fmt.Println("Success")
		} else {
			fmt.Printf("Failure: %v != %v\n", res, t.output)
		}
	}
}
