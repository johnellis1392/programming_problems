package main

import (
	"fmt"
	"os"
	"regexp"
	"strings"
)

func part1(input string) int {
	sum := 0
	re := regexp.MustCompile(`[^0-9]+`)

	for _, line := range strings.Split(input, "\n") {
		numbers := re.ReplaceAllString(line, "")
		if len(numbers) == 0 {
			continue
		}
		i := (int(numbers[0])-'0')*10 + (int(numbers[len(numbers)-1]) - '0')
		sum += i
	}

	return sum
}

func replace(input string) string {
	s := ""
	words := map[string]int{
		"one": 1, "two": 2, "three": 3, "four": 4,
		"five": 5, "six": 6, "seven": 7, "eight": 8, "nine": 9,
	}

	for i := 0; i < len(input); i++ {
		if '1' <= input[i] && input[i] <= '9' {
			s += fmt.Sprint(int(input[i] - '0'))
		}

		for k, v := range words {
			if strings.HasPrefix(input[i:], k) {
				s += fmt.Sprint(v)
				break
			}
		}
	}

	return s
}

func part2(input string) int {
	sum := 0
	for _, line := range strings.Split(input, "\n") {
		if len(line) == 0 {
			continue
		}
		line = strings.TrimSpace(line)
		numbers := replace(line)
		i := (int(numbers[0])-'0')*10 + (int(numbers[len(numbers)-1]) - '0')
		sum += i
	}

	return sum
}

func main() {
	// test_input := `1abc2
	// pqr3stu8vwx
	// a1b2c3d4e5f
	// treb7uchet`

	input, err := os.ReadFile("./input.txt")
	if err != nil {
		panic(err)
	}

	// fmt.Printf("part1 = %v\n", part1(test_input))
	fmt.Printf("part1 = %v\n", part1(string(input)))

	// test_input2 := `two1nine
	// eightwothree
	// abcone2threexyz
	// xtwone3four
	// 4nineeightseven2
	// zoneight234
	// 7pqrstsixteen`
	// fmt.Printf("part2 = %v\n", part2(test_input2))

	// 54878: Too Low
	fmt.Printf("part2 = %v\n", part2(string(input)))
}
