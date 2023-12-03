package main

import (
	"fmt"
	"os"
	"strings"
)

func priority(c byte) int {
	if 'a' <= c && c <= 'z' {
		return int(c - 'a' + 1)
	} else {
		return int(c - 'A' + 27)
	}
}

func part1(input string) int {
	sum := 0
	for _, line := range strings.Split(input, "\n") {
		line = strings.TrimSpace(line)
		if len(line) == 0 {
			continue
		}

		pivot := len(line) / 2
		var c byte
		left, right := line[0:pivot], line[len(line)-pivot:]

	loop:
		for l := 0; l < len(left); l++ {
			for r := 0; r < len(right); r++ {
				if left[l] == right[r] {
					c = left[l]
					break loop
				}
			}
		}

		sum += priority(c)
	}

	return sum
}

func intersection(s1, s2 string) string {
	res := ""
	for i := 0; i < len(s1); i++ {
		for j := 0; j < len(s2); j++ {
			if s1[i] == s2[j] {
				res += string(s1[i])
				break
			}
		}
	}
	return res
}

func part2(input string) int {
	lines := strings.Split(input, "\n")
	sum := 0

	for i := 0; i < len(lines); i += 3 {
		s := strings.TrimSpace(lines[i])
		s = intersection(s, strings.TrimSpace(lines[i+1]))
		s = intersection(s, strings.TrimSpace(lines[i+2]))
		sum += priority(s[0])
	}

	return sum
}

func main() {
	filename := "input.txt"
	const DEBUG = false
	test_input := `vJrwpWtwJgWrhcsFMMfFFhFp
	jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
	PmmdzqPrVvPwwTWBwg
	wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
	ttgJtRGJQctTZtZT
	CrZsJsPPZsGzwwsLwLmpwMDw`

	var input string
	if DEBUG {
		input = test_input
	} else {
		s, err := os.ReadFile(filename)
		if err != nil {
			panic(err)
		}
		input = string(s)
	}

	fmt.Printf("2022, Day 3, part 1: %v\n", part1(input))
	fmt.Printf("2022, Day 3, part 2: %v\n", part2(input))
}
