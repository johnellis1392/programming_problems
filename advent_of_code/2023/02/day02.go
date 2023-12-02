package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Game struct {
	id    int
	rolls [][]struct {
		n     int
		color string
	}
}

func readInput(input string) []Game {
	var games []Game
	for _, line := range strings.Split(input, "\n") {
		line = strings.TrimSpace(line)
		if len(line) == 0 {
			continue
		}
		var game Game

		var parts []string
		parts = strings.Split(line, ":")
		id, _ := strings.CutPrefix(parts[0], "Game ")
		idn, err := strconv.Atoi(id)
		if err != nil {
			panic(err)
		}
		game.id = idn

		var rolls [][]struct {
			n     int
			color string
		}

		for _, i := range strings.Split(parts[1], ";") {
			i = strings.TrimSpace(i)
			var rs []struct {
				n     int
				color string
			}

			for _, j := range strings.Split(i, ",") {
				j = strings.TrimSpace(j)
				roll := strings.Split(j, " ")
				n, err := strconv.Atoi(roll[0])
				if err != nil {
					panic(err)
				}
				color := roll[1]
				rs = append(rs, struct {
					n     int
					color string
				}{
					n:     n,
					color: color,
				})
			}
			rolls = append(rolls, rs)
		}

		game.rolls = rolls
		games = append(games, game)
	}
	return games
}

func dumpGame(game Game) {
	fmt.Printf("Game %d: ", game.id)
	for i, r := range game.rolls {
		if i != 0 {
			fmt.Printf("; ")
		}
		for j, s := range r {
			if j != 0 {
				fmt.Printf(", ")
			}
			fmt.Printf("%d %s", s.n, s.color)
		}
	}
	fmt.Println()
}

func part1(input string) int {
	games := readInput(input)

	total_reds := 12
	total_greens := 13
	total_blues := 14

	sum := 0

outer:
	for _, game := range games {
		for _, round := range game.rolls {
			for _, roll := range round {
				switch roll.color {
				case "red":
					if roll.n > total_reds {
						continue outer
					}
				case "green":
					if roll.n > total_greens {
						continue outer
					}
				case "blue":
					if roll.n > total_blues {
						continue outer
					}
				}
			}
		}
		sum += game.id
	}

	return sum
}

func part2(input string) int {
	games := readInput(input)
	sum := 0

	for _, game := range games {
		reds, greens, blues := 0, 0, 0
		for _, round := range game.rolls {
			for _, roll := range round {
				switch roll.color {
				case "red":
					reds = max(reds, roll.n)
				case "green":
					greens = max(greens, roll.n)
				case "blue":
					blues = max(blues, roll.n)
				}
			}
		}
		sum += reds * greens * blues
	}

	return sum
}

func main() {
	const DEBUG = false
	filename := "input.txt"
	test_input := `Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
	Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
	Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
	Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
	Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green`

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

	fmt.Printf("2023, Day 2, part 1: %v\n", part1(input))
	fmt.Printf("2023, Day 2, part 2: %v\n", part2(input))
}
