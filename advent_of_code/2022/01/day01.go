package main

import (
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

func part1(input string) int {
	res := 0
	sum := 0
	for _, line := range strings.Split(input, "\n") {
		line = strings.TrimSpace(line)
		if len(line) == 0 {
			res = max(res, sum)
			sum = 0
			continue
		}

		n, err := strconv.Atoi(line)
		if err != nil {
			panic(err)
		}

		sum += n
	}
	res = max(sum, res)
	return res
}

func part2(input string) int {
	var sums []int
	sum := 0
	for _, line := range strings.Split(input, "\n") {
		line = strings.TrimSpace(line)
		if len(line) == 0 {
			sums = append(sums, sum)
			sum = 0
			continue
		}

		n, err := strconv.Atoi(line)
		if err != nil {
			panic(err)
		}

		sum += n
	}

	sums = append(sums, sum)
	sort.Slice(sums, func(i, j int) bool {
		return sums[i] > sums[j]
	})

	sum = 0
	for i := 0; i < 3; i++ {
		sum += sums[i]
	}
	return sum
}

func main() {
	filename := "input.txt"
	test_input := `1000
	2000
	3000
	
	4000
	
	5000
	6000
	
	7000
	8000
	9000
	
	10000`
	var input string
	const DEBUG = false
	if DEBUG {
		input = test_input
	} else {
		s, err := os.ReadFile(filename)
		if err != nil {
			panic(err)
		}
		input = string(s)
	}

	fmt.Printf("2022 Day 1, part 1: %v\n", part1(input))
	fmt.Printf("2022 Day 1, part 2: %v\n", part2(input))
}
