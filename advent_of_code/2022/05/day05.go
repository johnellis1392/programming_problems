package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Stack struct {
	values []string
}

func NewStack() *Stack {
	s := Stack{}
	return &s
}

func (s *Stack) push(v string) {
	s.values = append(s.values, v)
}

func (s *Stack) pop() *string {
	if len(s.values) == 0 {
		return nil
	}
	n := len(s.values)
	v := s.values[n-1]
	s.values = s.values[0 : n-1]
	return &v
}

func (s *Stack) peek() *string {
	if len(s.values) == 0 {
		return nil
	}
	return &s.values[len(s.values)-1]
}

func (stack *Stack) str() string {
	s := "["
	for i := 0; i < len(stack.values); i++ {
		if i != 0 {
			s += ", "
		}
		s += stack.values[i]
	}
	s += "]"
	return s
}

func (stack *Stack) size() int {
	return len(stack.values)
}

func dumpStacks(stacks []Stack) {
	for i := 0; i < len(stacks); i++ {
		fmt.Printf("%d: %s\n", i+1, stacks[i].str())
	}
}

func part1(input string) string {
	lines := strings.Split(input, "\n")
	n := 0
	for len(lines[n]) != 0 {
		n++
	}

	nStacks := (len(lines[n-1]) + 1) / 4
	stacks := make([]Stack, nStacks)
	for i := 0; i < nStacks; i++ {
		stacks[i] = *NewStack()
	}

	for i := n - 2; i >= 0; i-- {
		for j := 0; j < nStacks; j++ {
			c := string(lines[i][j*4+1])
			if c != " " {
				stacks[j].push(c)
			}
		}
	}

	// dumpStacks(stacks)

	for i := n + 1; i < len(lines); i++ {
		move := strings.Split(lines[i], " ")
		n, _ := strconv.Atoi(move[1])
		from, _ := strconv.Atoi(move[3])
		to, _ := strconv.Atoi(move[5])
		from -= 1
		to -= 1

		for j := 0; j < n; j++ {
			stacks[to].push(*stacks[from].pop())
		}
	}

	res := ""
	for _, stack := range stacks {
		res += *stack.peek()
	}
	return res
}

func part2(input string) string {
	lines := strings.Split(input, "\n")
	n := 0
	for len(lines[n]) != 0 {
		n++
	}

	nStacks := (len(lines[n-1]) + 1) / 4
	stacks := make([]Stack, nStacks)
	for i := 0; i < nStacks; i++ {
		stacks[i] = *NewStack()
	}

	for i := n - 2; i >= 0; i-- {
		for j := 0; j < nStacks; j++ {
			c := string(lines[i][j*4+1])
			if c != " " {
				stacks[j].push(c)
			}
		}
	}

	temp := *NewStack()
	for i := n + 1; i < len(lines); i++ {
		move := strings.Split(lines[i], " ")
		n, _ := strconv.Atoi(move[1])
		from, _ := strconv.Atoi(move[3])
		to, _ := strconv.Atoi(move[5])
		from -= 1
		to -= 1

		for j := 0; j < n; j++ {
			temp.push(*stacks[from].pop())
		}
		for j := 0; j < n; j++ {
			stacks[to].push(*temp.pop())
		}
	}

	res := ""
	for _, stack := range stacks {
		res += *stack.peek()
	}
	return res
}

const test_input = `    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2`

func main() {
	const DEBUG = true
	filename := "input.txt"
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

	fmt.Printf("2022 Day 5, part 1: %v\n", part1(input))
	fmt.Printf("2022 Day 5, part 2: %v\n", part2(input))
}
