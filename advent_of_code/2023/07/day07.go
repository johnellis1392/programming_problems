package main

import (
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

type Hand struct {
	cards    string
	bid      int
	values   []int
	strength int
}

func (h Hand) String() string {
	return h.cards + " bid: " + strconv.Itoa(h.bid)
}

func (h Hand) Strength() int {
	return h.strength
}

func (h Hand) HighCard() int {
	return h.values[0]
}

func (h Hand) Less(other Hand) bool {
	for i := 0; i < len(h.values); i++ {
		if h.values[i] != other.values[i] {
			return h.values[i] < other.values[i]
		}
	}
	return false
}

func first(cards map[string]int) int {
	for _, v := range cards {
		return v
	}
	return -1
}

// Part 1 Implementation
// func CalcStrength(hand string) int {
// 	cards := make(map[string]int)
// 	for i := 0; i < len(hand); i++ {
// 		c := string(hand[i])
// 		if _, found := cards[c]; found {
// 			cards[c] += 1
// 		} else {
// 			cards[c] = 1
// 		}
// 	}
// 	n := len(cards)
// 	switch {
// 	case n == 1: // Five of a Kind
// 		return 6
// 	case n == 2:
// 		if i := first(cards); i == 4 || i == 1 { // Four of a Kind
// 			return 5
// 		} else { // Full House
// 			return 4
// 		}
// 	case n == 3:
// 		rank := 0
// 		for _, i := range cards {
// 			if i == 3 { // Three of a Kind
// 				rank = 3
// 				break
// 			} else if i == 2 { // Two Pair
// 				rank = 2
// 				break
// 			} else {
// 				continue
// 			}
// 		}
// 		return rank
// 	case n == 4: // One Pair
// 		return 1
// 	default: // High Card
// 		return 0
// 	}
// }

func values(m map[string]int) []int {
	var res []int
	for _, val := range m {
		res = append(res, val)
	}
	return res
}

func CalcStrength(hand string) int {
	numJacks := 0
	cards := make(map[string]int)
	for i := 0; i < len(hand); i++ {
		if hand[i] == 'J' {
			numJacks++
		} else {
			c := string(hand[i])
			if _, found := cards[c]; found {
				cards[c] += 1
			} else {
				cards[c] = 1
			}
		}
	}

	vs := values(cards)
	sort.Slice(vs, func(i, j int) bool { return vs[i] > vs[j] })
	switch {
	case numJacks == 5: // All Jacks
		return 6
	case vs[0]+numJacks == 5: // Five of a Kind
		return 6
	case vs[0]+numJacks == 4: // Four of a Kind
		return 5
	case vs[0]+numJacks == 3 && vs[1] == 2: // Full House
		return 4
	case vs[0]+numJacks == 3 && vs[1] == 1: // Three of a Kind
		return 3
	case vs[0]+numJacks == 2 && vs[1] == 2: // Two Pair
		return 2
	case vs[0]+numJacks == 2 && vs[1] == 1: // One Pair
		return 1
	default: // High Card
		return 0
	}
}

func cardRank(c byte) int {
	switch c {
	case 'A':
		return 14
	case 'K':
		return 13
	case 'Q':
		return 12
	// case 'J':
	// 	return 11
	case 'T':
		return 10
	case '9':
		return 9
	case '8':
		return 8
	case '7':
		return 7
	case '6':
		return 6
	case '5':
		return 5
	case '4':
		return 4
	case '3':
		return 3
	case '2':
		return 2
	case 'J':
		return 1
	default:
		return 0
	}
}

func NewHand(input string) Hand {
	parts := strings.Split(input, " ")
	bid, _ := strconv.Atoi(parts[1])
	var values []int
	for i := 0; i < len(parts[0]); i++ {
		values = append(values, cardRank(parts[0][i]))
	}
	return Hand{
		cards:    parts[0],
		bid:      bid,
		values:   values,
		strength: CalcStrength(parts[0]),
	}
}

func readInput(input string) []Hand {
	var hands []Hand
	for _, line := range strings.Split(input, "\n") {
		line = strings.TrimSpace(line)
		if len(line) == 0 {
			continue
		}
		hands = append(hands, NewHand(line))
	}
	return hands
}

func dump(hands []Hand) {
	for _, hand := range hands {
		fmt.Println(hand.String())
	}
}

func group(hands []Hand) [][]Hand {
	res := make([][]Hand, 7)
	for _, hand := range hands {
		s := hand.Strength()
		res[s] = append(res[s], hand)
	}
	return res
}

func part1(input string) int {
	hands := readInput(input)
	groups := group(hands)

	res := 0
	rank := 1
	for i := 0; i < 7; i++ {
		group := groups[i]
		sort.Slice(group, func(i, j int) bool {
			return group[i].Less(group[j])
		})

		for _, hand := range group {
			res += rank * hand.bid
			rank++
		}
	}

	return res
}

func part2(input string) int {
	hands := readInput(input)
	groups := group(hands)

	res := 0
	rank := 1
	for i := 0; i < 7; i++ {
		group := groups[i]
		sort.Slice(group, func(i, j int) bool {
			return group[i].Less(group[j])
		})

		for _, hand := range group {
			res += rank * hand.bid
			rank++
		}
	}

	return res
}

func main() {
	const DEBUG = false
	filename := "input.txt"
	test_input := `32T3K 765
	T55J5 684
	KK677 28
	KTJJT 220
	QQQJA 483`

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

	fmt.Printf("2023 Day 7, Part 1: %v\n", part1(input))
	fmt.Printf("2023 Day 7, Part 2: %v\n", part2(input))
}
