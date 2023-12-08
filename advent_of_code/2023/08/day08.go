package main

import (
	"fmt"
	"os"
	"strings"
)

type Direction struct {
	i    int
	dirs string
}

func NewDirection(s string) Direction {
	return Direction{0, s}
}

func (d *Direction) Reset() {
	d.i = 0
}

func (d *Direction) Next() string {
	dir := string(d.dirs[d.i])
	d.i++
	if d.i >= len(d.dirs) {
		d.i = 0
	}
	return dir
}

type Inst struct {
	id    string
	left  string
	right string
}

func (inst Inst) String() string {
	return fmt.Sprintf("%s = (%s, %s)", inst.id, inst.left, inst.right)
}

func (inst Inst) IsStart() bool {
	return inst.id[2] == 'A'
}

func (inst Inst) IsEnd() bool {
	return inst.id[2] == 'Z'
}

func readInput(input string) (Direction, map[string]Inst) {
	lines := strings.Split(input, "\n")
	dirLine := lines[0]
	dirLine = strings.TrimSpace(dirLine)

	insts := make(map[string]Inst)

	for i := 2; i < len(lines); i++ {
		line := strings.TrimSpace(lines[i])
		if len(line) == 0 {
			continue
		}

		parts := strings.Split(line, " = ")
		id := parts[0]
		dirString := strings.TrimSpace(parts[1])
		dirString = strings.TrimPrefix(dirString, "(")
		dirString = strings.TrimSuffix(dirString, ")")
		dirs := strings.Split(dirString, ", ")
		left, right := dirs[0], dirs[1]

		insts[id] = Inst{id, left, right}
	}

	return NewDirection(dirLine), insts
}

func dump(dirs Direction, insts map[string]Inst) {
	fmt.Printf("Directions: %s\n", dirs.dirs)
	fmt.Println()
	fmt.Println("Instructions:")
	for _, inst := range insts {
		fmt.Println(inst.String())
	}
}

func part1(input string) int {
	dirs, insts := readInput(input)
	// dump(dirs, insts)
	n := 0
	current := insts["AAA"]
	for current.id != "ZZZ" {
		nextDir := dirs.Next()
		if nextDir == "R" {
			current = insts[current.right]
		} else {
			current = insts[current.left]
		}
		n++
	}
	return n
}

func isEnd(insts []Inst) bool {
	for _, inst := range insts {
		if !inst.IsEnd() {
			return false
		}
	}
	return true
}

func gcd(a, b int) int {
	for b != 0 {
		t := b
		b = a % b
		a = t
	}
	return a
}

func lcm(vs []int) int {
	res := vs[0] * vs[1] / gcd(vs[0], vs[1])
	for i := 2; i < len(vs); i++ {
		res = res * vs[i] / gcd(res, vs[i])
	}
	return res
}

func part2(input string) int {
	dirs, insts := readInput(input)
	var currentInsts []Inst
	for _, inst := range insts {
		if inst.IsStart() {
			currentInsts = append(currentInsts, inst)
		}
	}

	// Rote solution, too inefficient to work
	// n := 0
	// for !isEnd(currentInsts) {
	// 	n++
	// 	dir := dirs.Next()
	// 	for i := 0; i < len(currentInsts); i++ {
	// 		inst := currentInsts[i]
	// 		if dir == "R" {
	// 			currentInsts[i] = insts[inst.right]
	// 		} else {
	// 			currentInsts[i] = insts[inst.left]
	// 		}
	// 	}
	// }

	// LCM solution; works like a charm
	endStates := make([]int, len(currentInsts))
	for i := 0; i < len(currentInsts); i++ {
		dirs.Reset()
		inst := currentInsts[i]
		for !inst.IsEnd() {
			dir := dirs.Next()
			endStates[i]++
			if dir == "R" {
				inst = insts[inst.right]
			} else {
				inst = insts[inst.left]
			}
		}
	}

	res := lcm(endStates)

	return res
}

func main() {
	const DEBUG = false
	filename := "input.txt"
	// test_input1 := `RL
	//
	// AAA = (BBB, CCC)
	// BBB = (DDD, EEE)
	// CCC = (ZZZ, GGG)
	// DDD = (DDD, DDD)
	// EEE = (EEE, EEE)
	// GGG = (GGG, GGG)
	// ZZZ = (ZZZ, ZZZ)
	// `

	test_input1 := `LLR
	
	AAA = (BBB, BBB)
	BBB = (AAA, ZZZ)
	ZZZ = (ZZZ, ZZZ)`

	test_input2 := `LR

	11A = (11B, XXX)
	11B = (XXX, 11Z)
	11Z = (11B, XXX)
	22A = (22B, XXX)
	22B = (22C, 22C)
	22C = (22Z, 22Z)
	22Z = (22B, 22B)
	XXX = (XXX, XXX)
	`

	if DEBUG {
		fmt.Printf("2023 Day 8, Part 1: %v\n", part1(test_input1))
		fmt.Printf("2023 Day 8, Part 2: %v\n", part2(test_input2))
	} else {
		s, err := os.ReadFile(filename)
		if err != nil {
			panic(err)
		}
		input := string(s)
		fmt.Printf("2023 Day 8, Part 1: %v\n", part1(input))
		fmt.Printf("2023 Day 8, Part 2: %v\n", part2(input))
	}

}
