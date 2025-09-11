package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
)

const win = 6
const loss = 0
const draw = 3
const rock = 1
const paper = 2
const scissors = 3

func day02_part1(filename string) int {
	file, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	result := 0
	for scanner.Scan() {
		line := scanner.Text()
		re := regexp.MustCompile(`^(\w) (\w)$`)
		ss := re.FindStringSubmatch(line)
		om, pm := ss[1], ss[2]
		switch {
		case (om == "A" && pm == "X"):
			result += draw + rock
			break
		case (om == "A" && pm == "Y"):
			result += win + paper
			break
		case (om == "A" && pm == "Z"):
			result += loss + scissors
			break
		case (om == "B" && pm == "X"):
			result += loss + rock
			break
		case (om == "B" && pm == "Y"):
			result += draw + paper
			break
		case (om == "B" && pm == "Z"):
			result += win + scissors
			break
		case (om == "C" && pm == "X"):
			result += win + rock
			break
		case (om == "C" && pm == "Y"):
			result += loss + paper
			break
		case (om == "C" && pm == "Z"):
			result += draw + scissors
			break
		}
	}

	return result
}

func day02_part2(filename string) int {
	file, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	result := 0
	for scanner.Scan() {
		line := scanner.Text()
		re := regexp.MustCompile(`^(\w) (\w)$`)
		ss := re.FindStringSubmatch(line)
		om, pm := ss[1], ss[2]
		switch {
		case (om == "A" && pm == "X"):
			result += loss + scissors
			break
		case (om == "A" && pm == "Y"):
			result += draw + rock
			break
		case (om == "A" && pm == "Z"):
			result += win + paper
			break
		case (om == "B" && pm == "X"):
			result += loss + rock
			break
		case (om == "B" && pm == "Y"):
			result += draw + paper
			break
		case (om == "B" && pm == "Z"):
			result += win + scissors
			break
		case (om == "C" && pm == "X"):
			result += loss + paper
			break
		case (om == "C" && pm == "Y"):
			result += draw + scissors
			break
		case (om == "C" && pm == "Z"):
			result += win + rock
			break
		}
	}

	return result
}

func main() {
	fmt.Println("Day 02")
	// input_filename := "input.test.txt"
	input_filename := "input.txt"
	result1 := day02_part1(input_filename)
	fmt.Println("Result1:", result1)
	result2 := day02_part2(input_filename)
	fmt.Println("Result2:", result2)
}
