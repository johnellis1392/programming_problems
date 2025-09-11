package main

import (
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

type Op interface {
	Eval(n uint64) uint64
	Value() string
	String() string
}

type PlusOp struct {
	i string
}

func (op PlusOp) Eval(n uint64) uint64 {
	if op.i == "old" {
		return n + n
	}
	v, _ := strconv.Atoi(op.i)
	return uint64(v) + n
}

func (op PlusOp) Value() string {
	return op.i
}

func (op PlusOp) String() string {
	return "+"
}

type MulOp struct {
	i string
}

func (op MulOp) Eval(n uint64) uint64 {
	if op.i == "old" {
		return n * n
	}
	v, _ := strconv.Atoi(op.i)
	return uint64(v) * n
}

func (op MulOp) Value() string {
	return op.i
}

func (op MulOp) String() string {
	return "*"
}

type Monkey struct {
	id                      int
	items                   []uint64
	operation               Op
	test                    uint64
	trueMonkey, falseMonkey int
	numInspections          uint64
}

func (m Monkey) String() string {
	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("Monkey %d:\n", m.id))
	sb.WriteString(fmt.Sprintf(" Starting Items: "))
	for i, item := range m.items {
		if i != 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(fmt.Sprint(item))
	}
	sb.WriteString("\n")
	sb.WriteString(fmt.Sprintf(" Operation: new = old %s %s\n", m.operation.String(), m.operation.Value()))
	sb.WriteString(fmt.Sprintf(" Test: divisible by %d\n", m.test))
	sb.WriteString(fmt.Sprintf("  If true: throw to monkey %d\n", m.trueMonkey))
	sb.WriteString(fmt.Sprintf("  If false: throw to monkey %d\n", m.falseMonkey))
	return sb.String()
}

func (m *Monkey) Inspect(reliefEnabled bool, divisor uint64) (uint64, bool) {
	if len(m.items) == 0 {
		panic("Monkey has no items to inspect")
	}

	worryLevel := m.items[0]
	m.items = m.items[1:]

	worryLevel = m.operation.Eval(worryLevel)
	if reliefEnabled {
		worryLevel /= 3
	}
	m.numInspections++
	worryLevel = worryLevel % divisor
	return worryLevel, worryLevel%m.test == 0
}

func (m *Monkey) Receive(v uint64) {
	m.items = append(m.items, v)
}

func readInput(input string) []Monkey {
	lines := strings.Split(input, "\n")
	var res []Monkey
	for i := 0; i < len(lines); i++ {
		monkeyLine := strings.TrimSpace(lines[i])
		if len(monkeyLine) == 0 {
			break
		}
		monkeyLine = strings.TrimPrefix(monkeyLine, "Monkey ")
		monkeyLine = monkeyLine[0 : len(monkeyLine)-1]
		monkeyId, _ := strconv.Atoi(monkeyLine)

		var items []uint64
		itemLine := strings.TrimSpace(lines[i+1])
		itemLine = strings.TrimPrefix(itemLine, "Starting items: ")
		itemStrings := strings.Split(itemLine, ", ")
		for _, s := range itemStrings {
			item, _ := strconv.Atoi(s)
			items = append(items, uint64(item))
		}

		opLine := strings.TrimSpace(lines[i+2])
		opLine = strings.TrimPrefix(opLine, "Operation: new = old ")
		var op Op
		n := opLine[2:]
		if opLine[0] == '+' {
			op = PlusOp{n}
		} else {
			op = MulOp{n}
		}

		testLine := strings.TrimSpace(lines[i+3])
		testLine = strings.TrimPrefix(testLine, "Test: divisible by ")
		test, _ := strconv.Atoi(testLine)

		trueLine := strings.TrimSpace(lines[i+4])
		trueLine = strings.TrimPrefix(trueLine, "If true: throw to monkey ")
		trueMonkey, _ := strconv.Atoi(trueLine)

		falseLine := strings.TrimSpace(lines[i+5])
		falseLine = strings.TrimPrefix(falseLine, "If false: throw to monkey ")
		falseMonkey, _ := strconv.Atoi(falseLine)

		res = append(res, Monkey{
			id:             monkeyId,
			items:          items,
			operation:      op,
			test:           uint64(test),
			trueMonkey:     trueMonkey,
			falseMonkey:    falseMonkey,
			numInspections: 0,
		})

		i += 6
	}

	return res
}

func dump(monkies []Monkey) {
	for _, monkey := range monkies {
		fmt.Println(monkey.String())
		fmt.Println()
	}
}

func part1(input string) uint64 {
	monkies := readInput(input)
	var divisor uint64 = 1
	for _, m := range monkies {
		divisor *= m.test
	}
	for round := 0; round < 20; round++ {
		for i := 0; i < len(monkies); i++ {
			monkey := &monkies[i]
			for len(monkey.items) > 0 {
				v, ok := monkey.Inspect(true, divisor)
				var target *Monkey
				if ok {
					target = &monkies[monkey.trueMonkey]
				} else {
					target = &monkies[monkey.falseMonkey]
				}
				target.Receive(v)
			}
		}
	}

	var inspections []uint64
	for _, monkey := range monkies {
		inspections = append(inspections, monkey.numInspections)
	}
	sort.Slice(inspections, func(i, j int) bool { return inspections[i] > inspections[j] })

	return inspections[0] * inspections[1]
}

func dumpInspections(round int, monkies []Monkey) {
	fmt.Printf("== After round %d ==\n", round)
	for _, monkey := range monkies {
		fmt.Printf("Monkey %d inspected items %d times.\n", monkey.id, monkey.numInspections)
	}
	fmt.Println()
}

func tryDumpInspections(round int, monkies []Monkey) {
	if round == 1 || round == 20 || round%1000 == 0 {
		dumpInspections(round, monkies)
	}
}

func part2(input string) uint64 {
	monkies := readInput(input)
	// dump(monkies)
	var divisor uint64 = 1
	for _, m := range monkies {
		divisor *= m.test
	}
	for round := 1; round <= 10000; round++ {
		for i := 0; i < len(monkies); i++ {
			monkey := &monkies[i]
			for len(monkey.items) > 0 {
				v, ok := monkey.Inspect(false, divisor)
				var target *Monkey
				if ok {
					target = &monkies[monkey.trueMonkey]
				} else {
					target = &monkies[monkey.falseMonkey]
				}
				target.Receive(v)
			}
		}
		// tryDumpInspections(round, monkies)
	}

	// dumpInspections(10000, monkies)
	var inspections []uint64
	for _, monkey := range monkies {
		inspections = append(inspections, monkey.numInspections)
	}
	sort.Slice(inspections, func(i, j int) bool { return inspections[i] > inspections[j] })

	return inspections[0] * inspections[1]
}

func main() {
	const DEBUG = false
	filename := "input.txt"
	test_input := `Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
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

	fmt.Printf("2022 Day 11, Part 1: %v\n", part1(input))
	fmt.Printf("2022 Day 11, Part 2: %v\n", part2(input))
}
