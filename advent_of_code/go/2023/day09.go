package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func readInput(input string) [][]int {
	var result [][]int
	for _, line := range strings.Split(input, "\n") {
		line = strings.TrimSpace(line)
		if len(line) == 0 {
			continue
		}
		var ns []int
		for _, v := range strings.Split(line, " ") {
			n, _ := strconv.Atoi(v)
			ns = append(ns, n)
		}
		result = append(result, ns)
	}
	return result
}

func dump(sequences [][]int) {
	for _, row := range sequences {
		for i, v := range row {
			if i != 0 {
				fmt.Printf(" ")
			}
			fmt.Printf("%d", v)
		}
		fmt.Println()
	}
}

func zeroes(sequence []int) bool {
	for _, v := range sequence {
		if v != 0 {
			return false
		}
	}
	return true
}

func differences(sequence []int) []int {
	diffs := make([]int, len(sequence)-1)
	for i := 0; i < len(diffs); i++ {
		diffs[i] = sequence[i+1] - sequence[i]
	}
	return diffs
}

func last(sequence []int) int {
	return sequence[len(sequence)-1]
}

func predict(sequence []int) int {
	if zeroes(sequence) {
		return 0
	}
	diffs := differences(sequence)
	next := predict(diffs)
	return last(sequence) + next
}

func part1(input string) int {
	sequences := readInput(input)
	sum := 0
	for _, sequence := range sequences {
		sum += predict(sequence)
	}
	return sum
}

func predictBackwards(sequence []int) int {
	if zeroes(sequence) {
		return 0
	}
	diffs := differences(sequence)
	prev := predictBackwards(diffs)
	return sequence[0] - prev
}

func part2(input string) int {
	sequences := readInput(input)
	sum := 0
	for _, sequence := range sequences {
		sum += predictBackwards(sequence)
	}
	return sum
}

func main() {
	const DEBUG = false
	filename := "input.txt"
	test_input := `0 3 6 9 12 15
	1 3 6 10 15 21
	10 13 16 21 30 45`

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

	fmt.Printf("2023 Day 9, Part 1: %v\n", part1(input))
	fmt.Printf("2023 Day 9, Part 2: %v\n", part2(input))
}
