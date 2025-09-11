package main

import (
	"fmt"
	"os"
	"strings"
)

func part1(input string) int {
	win_score := 6
	draw_score := 3
	lose_score := 0
	rock_score := 1
	paper_score := 2
	scissor_score := 3

	sum := 0
	for _, line := range strings.Split(input, "\n") {
		line = strings.TrimSpace(line)
		switch line {
		case "A X":
			sum += draw_score + rock_score
		case "A Y":
			sum += win_score + paper_score
		case "A Z":
			sum += lose_score + scissor_score
		case "B X":
			sum += lose_score + rock_score
		case "B Y":
			sum += draw_score + paper_score
		case "B Z":
			sum += win_score + scissor_score
		case "C X":
			sum += win_score + rock_score
		case "C Y":
			sum += lose_score + paper_score
		case "C Z":
			sum += draw_score + scissor_score
		}
	}

	return sum
}

func part2(input string) int {
	win_score := 6
	draw_score := 3
	lose_score := 0
	rock_score := 1
	paper_score := 2
	scissor_score := 3

	sum := 0

	for _, line := range strings.Split(input, "\n") {
		line = strings.TrimSpace(line)
		switch line {
		case "A X":
			sum += lose_score + scissor_score
		case "A Y":
			sum += draw_score + rock_score
		case "A Z":
			sum += win_score + paper_score
		case "B X":
			sum += lose_score + rock_score
		case "B Y":
			sum += draw_score + paper_score
		case "B Z":
			sum += win_score + scissor_score
		case "C X":
			sum += lose_score + paper_score
		case "C Y":
			sum += draw_score + scissor_score
		case "C Z":
			sum += win_score + rock_score
		}
	}

	return sum
}

func main() {
	filename := "input.txt"
	const DEBUG = false
	var input string
	if DEBUG {
		input = `A Y
		B X
		C Z`
	} else {
		s, err := os.ReadFile(filename)
		if err != nil {
			panic(err)
		}
		input = string(s)
	}

	fmt.Printf("2022 Day 2, part 1: %v\n", part1(input))
	fmt.Printf("2022 Day 2, part 2: %v\n", part2(input))
}
