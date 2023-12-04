package main

import (
	"fmt"
	"math"
	"os"
	"regexp"
	"strings"
)

type Card struct {
	id      string
	winning []string
	holding []string
}

func (c *Card) String() string {
	var s strings.Builder
	s.WriteString(fmt.Sprintf("Card %s: ", c.id))

	for i, n := range c.winning {
		if i > 0 {
			s.WriteString(" ")
		}
		s.WriteString(n)
	}

	s.WriteString(" | ")

	for i, n := range c.holding {
		if i > 0 {
			s.WriteString(" ")
		}
		s.WriteString(n)
	}

	return s.String()
}

func (card *Card) Score() int {
	n := 0
	for _, w := range card.winning {
		for _, h := range card.holding {
			if w == h {
				n++
			}
		}
	}
	return n
}

func dumpCards(cards []Card) {
	for _, card := range cards {
		fmt.Println(card.String())
	}
}

func readInput(input string) []Card {
	var res []Card
	re := regexp.MustCompile("[0-9]+")

	for _, line := range strings.Split(input, "\n") {
		line = strings.TrimSpace(line)
		if len(line) == 0 {
			continue
		}

		var parts []string
		parts = strings.Split(line, ":")
		id := strings.TrimPrefix(parts[0], "Card ")

		parts = strings.Split(parts[1], " | ")
		winning := re.FindAllString(parts[0], -1)
		holding := re.FindAllString(parts[1], -1)

		res = append(res, Card{
			id:      id,
			winning: winning,
			holding: holding,
		})
	}

	return res
}

func part1(input string) int {
	cards := readInput(input)
	res := 0

	for _, card := range cards {
		n := card.Score()
		if n != 0 {
			res += int(math.Pow(2, float64(n-1)))
		}
	}

	return res
}

func dumpTotals(cardTotals []int) {
	for i, total := range cardTotals {
		fmt.Printf("Card %d: %d\n", i+1, total)
	}
}

func part2(input string) int {
	cards := readInput(input)
	cardTotals := make([]int, len(cards))
	for i := 0; i < len(cardTotals); i++ {
		cardTotals[i] = 1
	}

	for i, card := range cards {
		n := card.Score()
		instances := cardTotals[i]
		for j := i + 1; j < i+n+1; j++ {
			cardTotals[j] += instances
		}
	}

	res := 0
	for _, total := range cardTotals {
		res += total
	}

	// dumpTotals(cardTotals)
	return res
}

func main() {
	const DEBUG = false
	filename := "input.txt"
	test_input := `Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
	Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
	Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
	Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
	Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
	Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11`

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

	fmt.Printf("2023 Day 4, Part 1: %v\n", part1(input))
	fmt.Printf("2023 Day 4, Part 2: %v\n", part2(input))
}
