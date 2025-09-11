package main

import (
	"fmt"
	"os"
)

func part1(input string) int {
outer:
	for i := 4; i < len(input); i++ {
		for j := i - 4; j < i-1; j++ {
			for k := j + 1; k < i; k++ {
				if input[j] == input[k] {
					continue outer
				}
			}
		}
		return i
	}
	return -1
}

func part2(input string) int {
	const n = 14
outer:
	for i := n; i < len(input); i++ {
		for j := i - n; j < i-1; j++ {
			for k := j + 1; k < i; k++ {
				if input[j] == input[k] {
					continue outer
				}
			}
		}
		return i
	}
	return -1
}

var tests []string = []string{
	"mjqjpqmgbljsphdztnvjfqwrcgsmlb",
	"bvwbjplbgvbhsrlpgdmjqwftvncz",
	"nppdvjthqldpwncqszvftbrmjlhg",
	"nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
	"zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw",
}

func main() {
	const DEBUG = true
	filename := "input.txt"
	var input string
	if DEBUG {
		fmt.Println("Part 1:")
		for _, test := range tests {
			fmt.Printf("%s = %d\n", test, part1(test))
		}
		fmt.Println("\nPart 2:")
		for _, test := range tests {
			fmt.Printf("%s = %d\n", test, part2(test))
		}
	} else {
		s, err := os.ReadFile(filename)
		if err != nil {
			panic(err)
		}
		input = string(s)

		fmt.Printf("2022, Day 6, part 1: %v\n", part1(input))
		fmt.Printf("2022, Day 6, part 2: %v\n", part2(input))
	}
}
