package main

import (
	"fmt"
	"regexp"
	"strings"
)

func format(s []string) string {
	var sb strings.Builder
	sb.WriteString("[")
	for i := 0; i < len(s); i++ {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(fmt.Sprintf("\"%s\"", s[i]))
	}
	sb.WriteString("]")
	return sb.String()
}

func LengthOfLastWord(s string) int {
	splits := regexp.MustCompile("\\s+").
		Split(strings.Trim(s, " "), -1)
	if len(splits) == 0 {
		return 0
	} else {
		return len(splits[len(splits)-1])
	}
}

// Another solution from LeetCode
func lengthOfLastWord2(s string) int {
	substrings := strings.Fields(s)
	return len(substrings[len(substrings)-1])
}

// Another solution that's actually a lot more imperative and
// straight-forward. It's basically a stripped-down state machine.
func lengthOfLastWord3(s string) int {
	n := 0
	for i := len(s) - 1; i >= 0; i-- {
		if s[i] == ' ' && n == 0 {
			continue
		} else if s[i] == ' ' && n != 0 {
			return n
		} else {
			n++
		}
	}
	return n
}

// func main() {
// 	fmt.Println("Running...")
// 	for _, t := range tests {
// 		res := LengthOfLastWord(t.s)
// 		if res == t.o {
// 			fmt.Println("Success")
// 		} else {
// 			fmt.Printf("Failure: %v != %v\n", res, t.o)
// 		}
// 	}
// }
