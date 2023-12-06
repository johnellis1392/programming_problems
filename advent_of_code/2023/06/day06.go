package main

import (
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func readInput(input string) ([]int, []int) {
	re := regexp.MustCompile("[0-9]+")
	lines := strings.Split(input, "\n")
	timeStrings := re.FindAllString(strings.TrimSpace(strings.TrimPrefix(lines[0], "Time:")), -1)
	distanceStrings := re.FindAllString(strings.TrimSpace(strings.TrimPrefix(lines[1], "Distance:")), -1)

	var times, distances []int

	for i := 0; i < len(timeStrings); i++ {
		time, _ := strconv.Atoi(timeStrings[i])
		distance, _ := strconv.Atoi(distanceStrings[i])
		times = append(times, time)
		distances = append(distances, distance)
	}

	return times, distances
}

func part1(input string) int {
	times, distances := readInput(input)
	res := 1
	for i := 0; i < len(times); i++ {
		time, distance := times[i], distances[i]
		n := 0
		for t := 1; t < time; t++ {
			speed := t
			dt := time - t
			dd := dt * speed
			if dd > distance {
				n++
			}
		}
		res *= n
	}
	return res
}

func readInput2(input string) (int, int) {
	lines := strings.Split(input, "\n")
	re := regexp.MustCompile("[0-9]")
	timeStrings := re.FindAllString(strings.TrimSpace(strings.TrimPrefix(lines[0], "Time:")), -1)
	distanceStrings := re.FindAllString(strings.TrimSpace(strings.TrimPrefix(lines[1], "Distance:")), -1)

	timeString := ""
	for _, ts := range timeStrings {
		timeString += ts
	}

	distanceString := ""
	for _, ds := range distanceStrings {
		distanceString += ds
	}

	time, _ := strconv.Atoi(timeString)
	distance, _ := strconv.Atoi(distanceString)

	return time, distance
}

func part2(input string) int {
	time, distance := readInput2(input)
	n := 0
	for t := 1; t < time; t++ {
		speed := t
		dt := time - t
		dd := dt * speed
		if dd > distance {
			n++
		}
	}
	return n
}

func main() {
	const DEBUG = false
	filename := "input.txt"
	test_input := `Time:      7  15   30
	Distance:  9  40  200`

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

	fmt.Printf("Day 6, Part 1: %v\n", part1(input))
	fmt.Printf("Day 6, Part 2: %v\n", part2(input))
}
