package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func part1(input string) int {
	res := 0
	for _, line := range strings.Split(input, "\n") {
		line = strings.TrimSpace(line)
		assignments := strings.Split(line, ",")
		a1, a2 := strings.Split(assignments[0], "-"), strings.Split(assignments[1], "-")
		x1, _ := strconv.Atoi(a1[0])
		y1, _ := strconv.Atoi(a1[1])
		x2, _ := strconv.Atoi(a2[0])
		y2, _ := strconv.Atoi(a2[1])
		if x1 <= x2 && y2 <= y1 {
			res++
		} else if x2 <= x1 && y1 <= y2 {
			res++
		}
	}
	return res
}

func part2(input string) int {
	res := 0
	for _, line := range strings.Split(input, "\n") {
		line = strings.TrimSpace(line)
		as := strings.Split(line, ",")
		p1, p2 := strings.Split(as[0], "-"), strings.Split(as[1], "-")
		x1, _ := strconv.Atoi(p1[0])
		y1, _ := strconv.Atoi(p1[1])
		x2, _ := strconv.Atoi(p2[0])
		y2, _ := strconv.Atoi(p2[1])
		if x1 <= x2 && x2 <= y1 || x1 <= y2 && y2 <= y1 {
			res += 1
		} else if x2 <= x1 && x1 <= y2 || x2 <= y1 && y1 <= y2 {
			res += 1
		}
	}
	return res
}

func main() {
	const DEBUG = false
	filename := "input.txt"
	test_input :=
		`2-4,6-8
	   2-3,4-5
	   5-7,7-9
	   2-8,3-7
	   6-6,4-6
	   2-6,4-8`
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
