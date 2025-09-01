package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Point struct {
	x, y int
}

func abs(x int) int {
	if x < 0 {
		return x * -1
	} else {
		return x
	}
}

func normalize(x int) int {
	if x == 0 {
		return 0
	} else if x < 0 {
		return -1
	} else {
		return 1
	}
}

func part1(input string) int {
	head := Point{0, 0}
	tail := Point{0, 0}
	lastPos := Point{0, 0}

	steps := make(map[Point]bool)
	steps[tail] = true

	for _, line := range strings.Split(input, "\n") {
		line = strings.TrimSpace(line)
		if len(line) == 0 {
			continue
		}

		move := strings.Split(line, " ")
		dir := move[0]
		n, _ := strconv.Atoi(move[1])

		var dx, dy int
		switch dir {
		case "R":
			dx, dy = 1, 0
		case "U":
			dx, dy = 0, 1
		case "L":
			dx, dy = -1, 0
		case "D":
			dx, dy = 0, -1
		}

		for i := 0; i < n; i++ {
			lastPos = head
			head.x += dx
			head.y += dy
			if abs(head.x-tail.x) > 1 || abs(head.y-tail.y) > 1 {
				tail = lastPos
				if _, ok := steps[tail]; !ok {
					steps[tail] = true
				}
			}
		}
	}

	return len(steps)
}

func dump(head Point, knots []Point, steps map[Point]bool, x1, y1, x2, y2 int) {
	w, h := x2-x1+2, y2-y1+2
	graph := make([][]string, h)
	for i := 0; i < h; i++ {
		graph[i] = make([]string, w)
	}

	for r := 0; r < h; r++ {
		for c := 0; c < w; c++ {
			graph[r][c] = "."
		}
	}

	for step, _ := range steps {
		x, y := step.x-x1+1, step.y-y1+1
		graph[y][x] = "#"
	}

	graph[-y1+1][-x1+1] = "s"

	for i := len(knots) - 1; i >= 0; i-- {
		point := knots[i]
		x, y := point.x-x1+1, point.y-y1+1
		graph[y][x] = fmt.Sprintf("%d", i+1)
	}

	graph[head.y-y1+1][head.x-x1+1] = "H"

	for r := h - 1; r >= 0; r-- {
		for c := 0; c < w; c++ {
			fmt.Printf("%s", graph[r][c])
		}
		fmt.Println()
	}
}

func part2(input string) int {
	x1, y1, x2, y2 := 0, 0, 0, 0
	head := Point{0, 0}
	knots := make([]Point, 9)
	for i := 0; i < 9; i++ {
		knots[i] = Point{0, 0}
	}
	// lastHeadPos := Point{0, 0}
	steps := make(map[Point]bool)
	steps[head] = true

	for _, line := range strings.Split(input, "\n") {
		line = strings.TrimSpace(line)
		if len(line) == 0 {
			continue
		}

		move := strings.Split(line, " ")
		dir := move[0]
		n, _ := strconv.Atoi(move[1])

		var dx, dy int
		switch dir {
		case "R":
			dx, dy = 1, 0
		case "L":
			dx, dy = -1, 0
		case "U":
			dx, dy = 0, 1
		case "D":
			dx, dy = 0, -1
		}

		for i := 0; i < n; i++ {
			// lastHeadPos = head
			head.x += dx
			head.y += dy
			// lastPos := lastHeadPos
			curr := head

			for j := 0; j < 9; j++ {
				if abs(curr.x-knots[j].x) <= 1 && abs(curr.y-knots[j].y) <= 1 {
					break
				}

				ddx, ddy := curr.x-knots[j].x, curr.y-knots[j].y
				ddx, ddy = normalize(ddx), normalize(ddy)
				knots[j].x += ddx
				knots[j].y += ddy

				curr = knots[j]
			}

			if _, ok := steps[knots[9-1]]; !ok {
				steps[knots[9-1]] = true
			}

			x1 = min(x1, head.x)
			x2 = max(x2, head.x)
			y1 = min(y1, head.y)
			y2 = max(y2, head.y)
		}
	}

	// dump(head, knots, steps, x1, y1, x2, y2)
	return len(steps)
}

func main() {
	const DEBUG = false
	filename := "input.txt"
	// test_input := `R 4
	// U 4
	// L 3
	// D 1
	// R 4
	// D 1
	// L 5
	// R 2`
	test_input := `R 5
	U 8
	L 8
	D 3
	R 17
	D 10
	L 25
	U 20`

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

	fmt.Printf("2022 Day 9, part 1: %v\n", part1(input))
	// 5012 too high
	fmt.Printf("2022 Day 9, part 2: %v\n", part2(input))
}
