package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func part1(input string) int {
	sum := 0
	clock := 0
	cycleCheck := 20
	x := 1
	for _, line := range strings.Split(input, "\n") {
		line = strings.TrimSpace(line)
		if len(line) == 0 {
			continue
		}

		if line == "noop" {
			clock++
			if clock >= cycleCheck {
				cycleCheck += 40
				sum += x * clock
			}
		} else {
			n, _ := strconv.Atoi(strings.Split(line, " ")[1])
			clock++
			if clock >= cycleCheck {
				cycleCheck += 40
				sum += x * clock
			}
			clock++
			if clock >= cycleCheck {
				cycleCheck += 40
				sum += x * clock
			}
			x += n
		}
	}
	return sum
}

func tick(clock *int, cycleCheck *int, x int) string {
	pixel := *clock % 40
	*clock++
	res := ""

	if x-1 <= pixel && pixel <= x+1 {
		res += "#"
	} else {
		res += "."
	}

	if *clock == *cycleCheck {
		*cycleCheck += 40
		res += "\n"
	}

	return res
}

func last(l []string) string {
	if len(l) == 0 {
		return ""
	} else {
		return l[len(l)-1]
	}
}

func lastLine(s string) string {
	return last(strings.Split(s, "\n"))
}

func renderSprite(x int) string {
	res := ""
	for i := 0; i < 40; i++ {
		if x-1 <= i && i <= x+1 {
			res += "#"
		} else {
			res += "."
		}
	}
	return res
}

func part2(input string) string {
	res := ""
	clock := 0
	cycleCheck := 40
	x := 1

	for _, line := range strings.Split(input, "\n") {
		line = strings.TrimSpace(line)
		if len(line) == 0 {
			continue
		}

		if line == "noop" {
			res += tick(&clock, &cycleCheck, x)
		} else {
			n, _ := strconv.Atoi(strings.Split(line, " ")[1])
			res += tick(&clock, &cycleCheck, x)
			res += tick(&clock, &cycleCheck, x)
			x += n
		}
	}

	return res
}

func main() {
	const DEBUG = true
	filename := "input.txt"
	test_input := `addx 15
	addx -11
	addx 6
	addx -3
	addx 5
	addx -1
	addx -8
	addx 13
	addx 4
	noop
	addx -1
	addx 5
	addx -1
	addx 5
	addx -1
	addx 5
	addx -1
	addx 5
	addx -1
	addx -35
	addx 1
	addx 24
	addx -19
	addx 1
	addx 16
	addx -11
	noop
	noop
	addx 21
	addx -15
	noop
	noop
	addx -3
	addx 9
	addx 1
	addx -3
	addx 8
	addx 1
	addx 5
	noop
	noop
	noop
	noop
	noop
	addx -36
	noop
	addx 1
	addx 7
	noop
	noop
	noop
	addx 2
	addx 6
	noop
	noop
	noop
	noop
	noop
	addx 1
	noop
	noop
	addx 7
	addx 1
	noop
	addx -13
	addx 13
	addx 7
	noop
	addx 1
	addx -33
	noop
	noop
	noop
	addx 2
	noop
	noop
	noop
	addx 8
	noop
	addx -1
	addx 2
	addx 1
	noop
	addx 17
	addx -9
	addx 1
	addx 1
	addx -3
	addx 11
	noop
	noop
	addx 1
	noop
	addx 1
	noop
	noop
	addx -13
	addx -19
	addx 1
	addx 3
	addx 26
	addx -30
	addx 12
	addx -1
	addx 3
	addx 1
	noop
	noop
	noop
	addx -9
	addx 18
	addx 1
	addx 2
	noop
	noop
	addx 9
	noop
	noop
	noop
	addx -1
	addx 2
	addx -37
	addx 1
	addx 3
	noop
	addx 15
	addx -21
	addx 22
	addx -6
	addx 1
	noop
	addx 2
	addx 1
	noop
	addx -10
	noop
	noop
	addx 20
	addx 1
	addx 2
	addx 2
	addx -6
	addx -11
	noop
	noop
	noop
	`

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

	fmt.Printf("2022 Day 10, Part 1: %v\n", part1(input))
	fmt.Printf("2022 Day 10, Part 2: \n\n%v\n", part2(input))
}
