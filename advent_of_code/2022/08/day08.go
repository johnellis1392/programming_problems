package main

import (
	"fmt"
	"os"
	"strings"
)

func readInput(input string) [][]int {
	lines := strings.Split(input, "\n")
	var res [][]int
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if len(line) == 0 {
			continue
		}
		var a []int
		for _, c := range line {
			a = append(a, int(c-'0'))
		}
		res = append(res, a)
	}
	return res
}

func copy(a [][]int) [][]int {
	res := make([][]int, len(a))
	for i, r := range a {
		res[i] = make([]int, len(r))
		for j, c := range r {
			res[i][j] = c
		}
	}
	return res
}

func newBoolArr(r, c int) [][]bool {
	res := make([][]bool, r)
	for i := 0; i < r; i++ {
		res[i] = make([]bool, c)
		for j := 0; j < c; j++ {
			res[i][j] = false
		}
	}
	return res
}

func part1(input string) int {
	a := readInput(input)
	m, n := len(a), len(a[0])

	var max int

	// From Left
	left := newBoolArr(m, n)
	for r := 0; r < m; r++ {
		max = -1
		for c := 0; c < n; c++ {
			if a[r][c] > max {
				left[r][c] = false
				max = a[r][c]
			} else {
				left[r][c] = true
			}
		}
	}

	// From Right
	right := newBoolArr(m, n)
	for r := 0; r < m; r++ {
		max = -1
		for c := n - 1; c >= 0; c-- {
			if a[r][c] > max {
				right[r][c] = false
				max = a[r][c]
			} else {
				right[r][c] = true
			}
		}
	}

	// From Top
	top := newBoolArr(m, n)
	for c := 0; c < n; c++ {
		max = -1
		for r := 0; r < m; r++ {
			if a[r][c] > max {
				top[r][c] = false
				max = a[r][c]
			} else {
				top[r][c] = true
			}
		}
	}

	// From Bottom
	bottom := newBoolArr(m, n)
	for c := 0; c < n; c++ {
		max = -1
		for r := m - 1; r >= 0; r-- {
			if a[r][c] > max {
				bottom[r][c] = false
				max = a[r][c]
			} else {
				bottom[r][c] = true
			}
		}
	}

	res := 0
	for r := 0; r < m; r++ {
		for c := 0; c < n; c++ {
			if left[r][c] && right[r][c] && top[r][c] && bottom[r][c] {
				res++
			}
		}
	}

	total := len(a) * len(a[0])
	return total - res
}

func scenicScore(a [][]int, r, c int) int {
	left := 0
	for i := c - 1; i >= 0; i-- {
		left++
		if a[r][i] >= a[r][c] {
			break
		}
	}

	right := 0
	for i := c + 1; i < len(a[r]); i++ {
		right++
		if a[r][i] >= a[r][c] {
			break
		}
	}

	top := 0
	for i := r - 1; i >= 0; i-- {
		top++
		if a[i][c] >= a[r][c] {
			break
		}
	}

	bottom := 0
	for i := r + 1; i < len(a); i++ {
		bottom++
		if a[i][c] >= a[r][c] {
			break
		}
	}

	return left * right * top * bottom
}

func part2(input string) int {
	a := readInput(input)
	res := 0
	for r := 0; r < len(a); r++ {
		for c := 0; c < len(a[r]); c++ {
			score := scenicScore(a, r, c)
			if score > res {
				res = score
			}
		}
	}
	return res
}

func main() {
	const DEBUG = false
	filename := "input.txt"
	test_input := `30373
	25512
	65332
	33549
	35390`

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

	fmt.Printf("2022 Day 8, part 1: %v\n", part1(input))
	fmt.Printf("2022 Day 8, part 2: %v\n", part2(input))
}
