package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func isDigit(matrix [][]byte, r, c int) bool {
	if r < 0 || c < 0 {
		return false
	} else if r >= len(matrix) || c >= len(matrix[r]) {
		return false
	} else {
		char := matrix[r][c]
		return '0' <= char && char <= '9'
	}
}

func isSymbol(matrix [][]byte, r, c int) bool {
	if r < 0 || c < 0 {
		return false
	} else if r >= len(matrix) || c >= len(matrix[0]) {
		return false
	} else if isDigit(matrix, r, c) {
		return false
	} else if matrix[r][c] == '.' {
		return false
	} else {
		return true
	}
}

func isPartNumber(matrix [][]byte, r, c int) bool {
	// Number Length
	var n int
	for i := c; i < len(matrix[r]) && isDigit(matrix, r, i); i++ {
		n++
	}

	for i := -1; i <= n; i++ {
		if isSymbol(matrix, r-1, c+i) {
			return true
		}
	}

	if isSymbol(matrix, r, c-1) {
		return true
	}

	if isSymbol(matrix, r, c+n) {
		return true
	}

	for i := -1; i <= n; i++ {
		if isSymbol(matrix, r+1, c+i) {
			return true
		}
	}

	return false
}

func readInput(input string) [][]byte {
	var matrix [][]byte
	for _, line := range strings.Split(input, "\n") {
		if len(line) == 0 {
			continue
		}
		line = strings.TrimSpace(line)
		matrix = append(matrix, []byte(line))
	}
	return matrix
}

func getNumFromMatrix(matrix [][]byte, r, c int) int {
	s := ""
	for i := c; i < len(matrix[r]) && isDigit(matrix, r, i); i++ {
		s += string(matrix[r][i])
	}
	n, err := strconv.Atoi(s)
	if err != nil {
		return 0
	} else {
		return n
	}
}

func part1(input string) int {
	sum := 0
	matrix := readInput(input)
	for r := 0; r < len(matrix); r++ {
		var c int = 0
		for c < len(matrix[r]) {
			if isDigit(matrix, r, c) {
				if isPartNumber(matrix, r, c) {
					sum += getNumFromMatrix(matrix, r, c)
				}
				for c < len(matrix[r]) && isDigit(matrix, r, c) {
					c++
				}
			} else {
				c++
			}
		}
	}
	return sum
}

func isGear(matrix [][]byte, r, c int) bool {
	if matrix[r][c] != '*' {
		return false
	}

	n := 0
	if r-1 >= 0 && isDigit(matrix, r-1, c-1) && isDigit(matrix, r-1, c+1) && matrix[r-1][c] == '.' {
		n += 2
	} else if isDigit(matrix, r-1, c-1) || isDigit(matrix, r-1, c) || isDigit(matrix, r-1, c+1) {
		n += 1
	}

	if isDigit(matrix, r, c-1) {
		n += 1
	}

	if isDigit(matrix, r, c+1) {
		n += 1
	}

	if r+1 < len(matrix) && isDigit(matrix, r+1, c-1) && isDigit(matrix, r+1, c+1) && matrix[r+1][c] == '.' {
		n += 2
	} else if isDigit(matrix, r+1, c-1) || isDigit(matrix, r+1, c) || isDigit(matrix, r+1, c+1) {
		n += 1
	}

	return n == 2
}

func getGearRatio(matrix [][]byte, r, c int) int {
	res := 1

	// Check both digits in top row
	if isDigit(matrix, r-1, c-1) && isDigit(matrix, r-1, c+1) && !isDigit(matrix, r-1, c) {
		ci := c - 1
		for ci >= 0 && isDigit(matrix, r-1, ci) {
			ci--
		}
		ci++
		res *= getNumFromMatrix(matrix, r-1, ci)

		ci = c + 1
		res *= getNumFromMatrix(matrix, r-1, ci)
		return res
	}

	// Check both digits in bottom row
	if isDigit(matrix, r+1, c-1) && isDigit(matrix, r+1, c+1) && !isDigit(matrix, r+1, c) {
		ci := c - 1
		for ci >= 0 && isDigit(matrix, r+1, ci) {
			ci--
		}
		ci++
		res *= getNumFromMatrix(matrix, r+1, ci)

		ci = c + 1
		res *= getNumFromMatrix(matrix, r+1, ci)
		return res
	}

	// Check top row
	if isDigit(matrix, r-1, c-1) {
		ci := c - 1
		for ci >= 0 && isDigit(matrix, r-1, ci) {
			ci--
		}
		ci++
		res *= getNumFromMatrix(matrix, r-1, ci)
	} else if isDigit(matrix, r-1, c) {
		res *= getNumFromMatrix(matrix, r-1, c)
	} else if isDigit(matrix, r-1, c+1) {
		res *= getNumFromMatrix(matrix, r-1, c+1)
	}

	// Check middle row
	if isDigit(matrix, r, c-1) {
		ci := c - 1
		for ci >= 0 && isDigit(matrix, r, ci) {
			ci--
		}
		ci++
		res *= getNumFromMatrix(matrix, r, ci)
	}
	if isDigit(matrix, r, c+1) {
		res *= getNumFromMatrix(matrix, r, c+1)
	}

	// Check bottom row
	if isDigit(matrix, r+1, c-1) {
		ci := c - 1
		for ci >= 0 && isDigit(matrix, r+1, ci) {
			ci--
		}
		ci++
		res *= getNumFromMatrix(matrix, r+1, ci)
	} else if isDigit(matrix, r+1, c) {
		res *= getNumFromMatrix(matrix, r+1, c)
	} else if isDigit(matrix, r+1, c+1) {
		res *= getNumFromMatrix(matrix, r+1, c+1)
	}

	return res
}

func part2(input string) int {
	sum := 0
	matrix := readInput(input)
	for r := 0; r < len(matrix); r++ {
		for c := 0; c < len(matrix[r]); c++ {
			if isGear(matrix, r, c) {
				sum += getGearRatio(matrix, r, c)
			}
		}
	}
	return sum
}

func main() {
	const DEBUG = false
	filename := "input.txt"
	test_input :=
		`467..114..
		 ...*......
		 ..35..633.
		 ......#...
		 617*......
		 .....+.58.
		 ..592.....
		 ......755.
		 ...$.*....
		 .664.598..`

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
	fmt.Printf("2023, Day 3, part 1: %v\n", part1(input))
	fmt.Printf("2023, Day 3, part 2: %v\n", part2(input))
}
