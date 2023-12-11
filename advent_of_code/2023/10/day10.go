package main

import (
	"fmt"
	"math"
	"os"
	"strings"
)

type Point struct {
	r, c int
}

func (p Point) String() string {
	return fmt.Sprintf("(%d, %d)", p.r, p.c)
}

func (p Point) West() Point {
	return Point{p.r, p.c - 1}
}

func (p Point) East() Point {
	return Point{p.r, p.c + 1}
}

func (p Point) North() Point {
	return Point{p.r - 1, p.c}
}

func (p Point) South() Point {
	return Point{p.r + 1, p.c}
}

type Grid struct {
	width, height int
	matrix        [][]string
	steps         [][]int
	start         Point
}

func (g *Grid) Size() (int, int) {
	return g.width, g.height
}

func (g *Grid) SetStep(p Point, i int) {
	g.steps[p.r][p.c] = i
}

func (g *Grid) GetStep(p Point) int {
	return g.steps[p.r][p.c]
}

func (g *Grid) Get(p Point) string {
	return g.matrix[p.r][p.c]
}

func (g *Grid) Reset() {
	width, height := g.Size()
	for r := 0; r < height; r++ {
		g.steps[r] = make([]int, width)
		for c := 0; c < width; c++ {
			g.steps[r][c] = math.MaxInt
		}
	}
	g.SetStep(g.start, 0)
}

func (g *Grid) Valid(p Point) bool {
	width, height := g.Size()
	return 0 <= p.r && p.r < height && 0 <= p.c && p.c < width
}

func (g *Grid) GetConnectedWalls(p Point) []Point {
	var res []Point
	r, c := p.r, p.c

	up := Point{r - 1, c}
	down := Point{r + 1, c}
	left := Point{r, c - 1}
	right := Point{r, c + 1}

	switch g.Get(p) {
	case "S":
		if g.Valid(up) {
			switch g.Get(up) {
			case "|", "7", "F":
				res = append(res, up)
			}
		}
		if g.Valid(down) {
			switch g.Get(down) {
			case "|", "J", "L":
				res = append(res, down)
			}
		}
		if g.Valid(left) {
			switch g.Get(left) {
			case "-", "L", "F":
				res = append(res, left)
			}
		}
		if g.Valid(right) {
			switch g.Get(right) {
			case "-", "J", "7":
				res = append(res, right)
			}
		}

	case "|":
		if g.Valid(up) {
			res = append(res, up)
		}
		if g.Valid(down) {
			res = append(res, down)
		}

	case "-":
		if g.Valid(left) {
			res = append(res, left)
		}
		if g.Valid(right) {
			res = append(res, right)
		}

	case "L":
		if g.Valid(up) {
			res = append(res, up)
		}
		if g.Valid(right) {
			res = append(res, right)
		}

	case "J":
		if g.Valid(up) {
			res = append(res, up)
		}
		if g.Valid(left) {
			res = append(res, left)
		}

	case "7":
		if g.Valid(left) {
			res = append(res, left)
		}
		if g.Valid(down) {
			res = append(res, down)
		}

	case "F":
		if g.Valid(right) {
			res = append(res, right)
		}
		if g.Valid(down) {
			res = append(res, down)
		}

	default:
		// No-op
	}
	return res
}

func (g *Grid) FewerSteps(from, to Point) bool {
	return g.GetStep(from)+1 < g.GetStep(to)
}

func (g *Grid) Step(from, to Point) {
	g.SetStep(to, g.GetStep(from)+1)
}

func (g *Grid) Adjacents(p Point) []Point {
	var res []Point
	up := Point{p.r - 1, p.c}
	if g.Valid(up) {
		res = append(res, up)
	}
	down := Point{p.r + 1, p.c}
	if g.Valid(down) {
		res = append(res, down)
	}
	left := Point{p.r, p.c - 1}
	if g.Valid(left) {
		res = append(res, left)
	}
	right := Point{p.r, p.c + 1}
	if g.Valid(right) {
		res = append(res, right)
	}
	return res
}

func NewGrid(matrix [][]string) *Grid {
	height, width := len(matrix), len(matrix[0])
	steps := make([][]int, height)
	var start Point

outer:
	for r := 0; r < height; r++ {
		for c := 0; c < width; c++ {
			if matrix[r][c] == "S" {
				start = Point{r, c}
				break outer
			}
		}
	}

	grid := &Grid{
		width:  width,
		height: height,
		matrix: matrix,
		steps:  steps,
		start:  start,
	}
	grid.Reset()
	return grid
}

func readInput(input string) *Grid {
	var matrix [][]string
	for _, line := range strings.Split(input, "\n") {
		line = strings.TrimSpace(line)
		if len(line) == 0 {
			continue
		}
		var row []string
		for i := 0; i < len(line); i++ {
			row = append(row, string(line[i]))
		}
		matrix = append(matrix, row)
	}
	return NewGrid(matrix)
}

type Queue struct {
	vs []Point
}

func (q *Queue) Enqueue(p Point) {
	q.vs = append(q.vs, p)
}

func (q *Queue) Pop() Point {
	v := q.vs[0]
	q.vs = q.vs[1:]
	return v
}

func (q *Queue) Empty() bool {
	return len(q.vs) == 0
}

func NewQueue() *Queue {
	return &Queue{[]Point{}}
}

type Set struct {
	vs map[Point]bool
}

func NewSet() *Set {
	return &Set{make(map[Point]bool)}
}

func (s *Set) First() Point {
	for p := range s.vs {
		return p
	}
	return Point{-1, -1}
}

func (s *Set) Add(p Point) {
	s.vs[p] = true
}

func (s *Set) Remove(p Point) {
	delete(s.vs, p)
}

func (s *Set) Contains(p Point) bool {
	_, ok := s.vs[p]
	return ok
}

func (s *Set) Diff(o *Set) (res *Set) {
	res = NewSet()
	s.ForEach(func(p Point) {
		if !o.Contains(p) {
			res.Add(p)
		}
	})
	return res
}

func (s *Set) Union(o *Set) (res *Set) {
	res = NewSet()
	s.ForEach(func(p Point) {
		res.Add(p)
	})
	o.ForEach(func(p Point) {
		res.Add(p)
	})
	return res
}

func (s *Set) Xor(o *Set) (res *Set) {
	res = NewSet()
	s.ForEach(func(p Point) {
		if !o.Contains(p) {
			res.Add(p)
		}
	})
	o.ForEach(func(p Point) {
		if !s.Contains(p) {
			res.Add(p)
		}
	})
	return res
}

func (s *Set) Intersect(o *Set) (res *Set) {
	res = NewSet()
	s.ForEach(func(p Point) {
		if o.Contains(p) {
			res.Add(p)
		}
	})
	return res
}

func (s *Set) Size() int {
	return len(s.vs)
}

func (s *Set) Overlaps(o *Set) bool {
	return s.Intersect(o).Size() > 0
}

func (s *Set) ForEach(f func(p Point)) {
	for p := range s.vs {
		f(p)
	}
}

func part1(input string) int {
	grid := readInput(input)
	frontier := NewQueue()
	frontier.Enqueue(grid.start)

	res := -1
	for !frontier.Empty() {
		currentPos := frontier.Pop()
		res = max(res, grid.GetStep(currentPos))
		for _, p := range grid.GetConnectedWalls(currentPos) {
			if grid.FewerSteps(currentPos, p) {
				frontier.Enqueue(p)
				grid.Step(currentPos, p)
			}
		}
	}

	return res
}

func getWalls(grid *Grid) *Set {
	frontier := NewQueue()
	frontier.Enqueue(grid.start)
	walls := NewSet()

	for !frontier.Empty() {
		currentPos := frontier.Pop()
		for _, p := range grid.GetConnectedWalls(currentPos) {
			if !walls.Contains(p) {
				walls.Add(p)
				frontier.Enqueue(p)
			}
		}
	}

	return walls
}

func getTiles(grid *Grid, walls *Set) *Set {
	w, h := grid.Size()
	tiles := NewSet()
	for r := 0; r < h; r++ {
		for c := 0; c < w; c++ {
			p := Point{r, c}
			if !walls.Contains(p) {
				tiles.Add(p)
			}
		}
	}
	return tiles
}

func contained(grid *Grid, walls *Set, p Point) bool {
	res := 0

	_, h := grid.Size()
	r := p.r
	for ; r < h; r++ {
		if !walls.Contains(Point{r, p.c}) {
			continue
		}
		s := grid.Get(Point{r, p.c})
		switch s {
		case "-":
			res++
		case "F":
			res++
			r++
			for ; r < h; r++ {
				s2 := grid.Get(Point{r, p.c})
				if s2 == "J" {
					break
				} else if s2 == "L" {
					res++
					break
				}
			}
		case "7":
			res++
			r++
			for ; r < h; r++ {
				s2 := grid.Get(Point{r, p.c})
				if s2 == "L" {
					break
				} else if s2 == "J" {
					res++
					break
				}
			}
		}
	}

	return res%2 == 1
}

func replaceStart(grid *Grid) {
	p := grid.start
	ps := grid.GetConnectedWalls(p)
	a, b := ps[0], ps[1]

	eq := func(q1, q2 Point) bool {
		return a == q1 && b == q2 || b == q2 && a == q1
	}

	var s string
	switch {
	case eq(p.South(), p.North()):
		s = "|"
	case eq(p.West(), p.East()):
		s = "-"
	case eq(p.West(), p.North()):
		s = "J"
	case eq(p.North(), p.East()):
		s = "L"
	case eq(p.East(), p.South()):
		s = "F"
	case eq(p.South(), p.West()):
		s = "7"
	}
	grid.matrix[p.r][p.c] = s
}

func part2(input string) int {
	grid := readInput(input)
	walls := getWalls(grid)
	tiles := getTiles(grid, walls)
	replaceStart(grid)

	sum := 0
	containedPoints := NewSet()
	tiles.ForEach(func(p Point) {
		if contained(grid, walls, p) {
			sum++
			containedPoints.Add(p)
		}
	})

	return sum
}

func main() {
	const DEBUG = false
	filename := "input.txt"
	test_input :=
		`.....
	   .S-7.
	   .|.|.
	   .L-J.
	   .....`

	// test_input =
	// 	`FF7FSF7F7F7F7F7F---7
	// 	 L|LJ||||||||||||F--J
	// 	 FL-7LJLJ||||||LJL-77
	// 	 F--JF--7||LJLJ7F7FJ-
	// 	 L---JF-JLJ.||-FJLJJ7
	// 	 |F|F-JF---7F7-L7L|7|
	// 	 |FFJF7L7F-JF7|JL---7
	// 	 7-L-JL7||F7|L7F-7F7|
	// 	 L.L7LFJ|||||FJL7||LJ
	// 	 L7JLJL-JLJLJL--JLJ.L`

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

	fmt.Printf("2023 Day 10, Part 1: %v\n", part1(input))
	fmt.Printf("2023 Day 10, Part 2: %v\n", part2(input))
}
