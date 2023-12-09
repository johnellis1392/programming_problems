package main

import (
	"fmt"
	"math"
	"os"
	"strings"
)

func abs(i int) int {
	if i < 0 {
		return i * -1
	} else {
		return i
	}
}

func height(c byte) int {
	return int(c - 'a')
}

type Point struct {
	r, c int
}

type Grid struct {
	matrix           [][]string
	heightMap        [][]int
	steps            [][]int
	start, end       Point
	numRows, numCols int
}

func NewGrid(input [][]byte) *Grid {
	m, n := len(input), len(input[0])
	matrix := make([][]string, m)
	heightMap := make([][]int, m)
	steps := make([][]int, m)
	var start, end Point

	for r := 0; r < m; r++ {
		matrix[r] = make([]string, n)
		heightMap[r] = make([]int, n)
		steps[r] = make([]int, n)
		for c := 0; c < n; c++ {
			matrix[r][c] = string(input[r][c])
			steps[r][c] = math.MaxInt
			if input[r][c] == 'S' {
				heightMap[r][c] = height('a')
				start = Point{r, c}
			} else if input[r][c] == 'E' {
				heightMap[r][c] = height('z')
				end = Point{r, c}
			} else {
				heightMap[r][c] = height(input[r][c])
			}
		}
	}

	return &Grid{
		matrix:    matrix,
		heightMap: heightMap,
		steps:     steps,
		start:     start,
		end:       end,
		numRows:   m,
		numCols:   n,
	}
}

func (g *Grid) Get(p Point) (string, int) {
	return g.matrix[p.r][p.c], g.heightMap[p.r][p.c]
}

func (g *Grid) Size() (int, int) {
	return g.numRows, g.numCols
}

func (g *Grid) Reset() {
	m, n := g.Size()
	for r := 0; r < m; r++ {
		for c := 0; c < n; c++ {
			g.steps[r][c] = math.MaxInt
		}
	}
}

func (g *Grid) StartingPoints() []Point {
	var res []Point
	m, n := g.Size()
	for r := 0; r < m; r++ {
		for c := 0; c < n; c++ {
			if g.matrix[r][c] == "a" {
				res = append(res, Point{r, c})
			}
		}
	}
	return res
}

func (g *Grid) Adjacents(p Point) []Point {
	var adjacents []Point
	numRows, numCols := g.Size()
	if p.r-1 >= 0 {
		adjacents = append(adjacents, Point{p.r - 1, p.c})
	}
	if p.r+1 < numRows {
		adjacents = append(adjacents, Point{p.r + 1, p.c})
	}
	if p.c-1 >= 0 {
		adjacents = append(adjacents, Point{p.r, p.c - 1})
	}
	if p.c+1 < numCols {
		adjacents = append(adjacents, Point{p.r, p.c + 1})
	}
	return adjacents
}

func (g *Grid) Moveable(from, to Point) bool {
	_, h1 := g.Get(from)
	_, h2 := g.Get(to)
	if h2 <= h1 || h2-h1 == 1 {
		return true
	} else {
		return false
	}
}

func (g *Grid) FewerSteps(from, to Point) bool {
	s1 := g.GetStep(from)
	s2 := g.GetStep(to)
	if s1+1 < s2 {
		return true
	} else {
		return false
	}
}

func (g *Grid) GetStep(p Point) int {
	return g.steps[p.r][p.c]
}

func (g *Grid) SetStep(p Point, i int) {
	g.steps[p.r][p.c] = i
}

func (g *Grid) Step(from, to Point) {
	g.SetStep(to, g.GetStep(from)+1)
}

func readInput(input string) *Grid {
	lines := strings.Split(input, "\n")
	var res [][]byte
	for r := 0; r < len(lines); r++ {
		line := strings.TrimSpace(lines[r])
		if len(line) == 0 {
			continue
		}
		res = append(res, make([]byte, len(line)))
		for c := 0; c < len(line); c++ {
			res[r][c] = line[c]
		}
	}
	return NewGrid(res)
}

func dumpMatrix(grid *Grid) {
	m, n := grid.Size()
	for r := 0; r < m; r++ {
		for c := 0; c < n; c++ {
			fmt.Printf("%v", grid.matrix[r][c])
		}
		fmt.Println()
	}
}

func dumpHeightMap(grid *Grid) {
	m, n := grid.Size()
	for r := 0; r < m; r++ {
		for c := 0; c < n; c++ {
			if c != 0 {
				fmt.Printf(" ")
			}
			fmt.Printf("%2d", grid.heightMap[r][c])
		}
		fmt.Println()
	}
}

func dumpSteps(grid *Grid) {
	m, n := grid.Size()
	for r := 0; r < m; r++ {
		for c := 0; c < n; c++ {
			if c != 0 {
				fmt.Printf(" ")
			}
			v := grid.steps[r][c]
			if v == math.MaxInt {
				v = -1
			}
			fmt.Printf("%2d", v)
		}
		fmt.Println()
	}
}

func dumpByLetter(grid *Grid, s string) {
	m, n := grid.Size()
	for r := 0; r < m; r++ {
		for c := 0; c < n; c++ {
			if c != 0 {
				fmt.Printf(" ")
			}
			str, _ := grid.Get(Point{r, c})
			if str == s {
				step := grid.GetStep(Point{r, c})
				if step == math.MaxInt {
					fmt.Printf("%2d", -1)
				} else {
					fmt.Printf("%2d", grid.GetStep(Point{r, c}))
				}
			} else {
				fmt.Printf("%2d", 0)
			}
		}
		fmt.Println()
	}
}

func dump(grid *Grid) {
	dumpMatrix(grid)
	fmt.Println()
	dumpHeightMap(grid)
	fmt.Println()
	dumpSteps(grid)
}

type Queue struct {
	q []Point
}

func NewQueue() *Queue {
	return &Queue{[]Point{}}
}

func (q *Queue) Enqueue(p Point) {
	q.q = append(q.q, p)
}

func (q *Queue) Pop() Point {
	p := q.q[0]
	q.q = q.q[1:]
	return p
}

func (q *Queue) Empty() bool {
	return len(q.q) == 0
}

func shortestPath(grid *Grid, start, end Point) int {
	grid.SetStep(start, 0)

	frontier := NewQueue()
	frontier.Enqueue(start)

	for !frontier.Empty() {
		currentPos := frontier.Pop()
		for _, p := range grid.Adjacents(currentPos) {
			if grid.Moveable(currentPos, p) && grid.FewerSteps(currentPos, p) {
				grid.Step(currentPos, p)
				frontier.Enqueue(p)
			}
		}
	}

	return grid.GetStep(end)
}

func part1(input string) int {
	grid := readInput(input)
	start, end := grid.start, grid.end
	grid.Reset()
	return shortestPath(grid, start, end)
}

func part2(input string) int {
	grid := readInput(input)
	end := grid.end
	n := math.MaxInt
	for _, start := range grid.StartingPoints() {
		n = min(n, shortestPath(grid, start, end))
	}
	return n
}

func main() {
	const DEBUG = false
	filename := "input.txt"
	test_input := `Sabqponm
	abcryxxl
	accszExk
	acctuvwj
	abdefghi`

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

	fmt.Printf("2022 Day 12, Part 1: %v\n", part1(input))
	fmt.Printf("2022 Day 12, Part 2: %v\n", part2(input))
}
