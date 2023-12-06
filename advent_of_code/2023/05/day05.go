package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
)

type Range struct {
	start int
	size  int
}

func (r Range) Start() int {
	return r.start
}

func (r Range) End() int {
	return r.start + r.size
}

type RangeMap struct {
	sourceStart int
	destStart   int
	size        int
}

func (r RangeMap) SourceStart() int {
	return r.sourceStart
}

func (r RangeMap) SourceEnd() int {
	return r.sourceStart + r.size
}

func (r RangeMap) DestStart() int {
	return r.destStart
}

func (r RangeMap) DestEnd() int {
	return r.destStart + r.size
}

func (r RangeMap) String() string {
	return fmt.Sprintf("[%d, %d; %d]", r.sourceStart, r.destStart, r.size)
}

func (r RangeMap) Transform(n int) int {
	return n - r.sourceStart + r.destStart
}

func (r RangeMap) Contains(n int) bool {
	if r.sourceStart <= n && n < r.sourceStart+r.size {
		return true
	}
	return false
}

// Assumes the rng is fully contained by r
func (r RangeMap) TransformRange(rng Range) Range {
	if r.Contains(rng.Start()) {
		return Range{r.DestStart(), rng.size}
	} else {
		return rng
	}
}

func (r RangeMap) SplitRange(rng Range) []Range {
	if r.SourceStart() <= rng.Start() && rng.End() <= r.SourceEnd() {
		return []Range{
			{r.Transform(rng.start), rng.size},
		}
	} else if rng.Start() <= r.SourceStart() && r.SourceEnd() <= rng.End() {
		return []Range{
			{rng.start, r.sourceStart - rng.start},
			{r.sourceStart, r.size},
			{r.SourceEnd(), rng.End() - r.SourceEnd()},
		}
	} else if r.SourceStart() == rng.Start() && r.SourceEnd() < rng.End() {
		return []Range{
			{r.SourceStart(), r.SourceEnd() - r.SourceStart()},
			{r.SourceEnd(), rng.End()},
		}
	} else if rng.Start() < r.SourceStart() && r.SourceEnd() == rng.End() {
		return []Range{
			{rng.Start(), r.SourceStart() - rng.Start()},
			{r.SourceStart(), r.SourceEnd() - r.SourceStart()},
		}
	} else if r.SourceStart() < rng.Start() && r.SourceEnd() < rng.End() {
		return []Range{
			{r.SourceStart(), rng.Start() - r.SourceStart()},
			{rng.Start(), r.SourceEnd() - rng.Start()},
			{r.SourceEnd(), rng.End()},
		}
	} else if rng.Start() < r.SourceStart() && rng.End() < r.SourceEnd() {
		return []Range{
			{rng.Start(), r.SourceStart() - rng.Start()},
			{r.SourceStart(), rng.End() - r.SourceStart()},
			{rng.End(), r.SourceEnd() - rng.End()},
		}
	} else {
		// Otherwise, no overlap
		return []Range{rng}
	}
}

type SourceMap struct {
	sourceType string
	destType   string
	ranges     []RangeMap
}

func (s SourceMap) String() string {
	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("%s-to-%s map:\n", s.sourceType, s.destType))
	for _, r := range s.ranges {
		sb.WriteString(fmt.Sprintf("%s\n", r.String()))
	}
	return sb.String()
}

func (s SourceMap) Transform(n int) int {
	var r *RangeMap = nil

	for _, rng := range s.ranges {
		if rng.Contains(n) {
			r = &rng
			break
		}
	}

	if r == nil {
		return n
	} else {
		return r.Transform(n)
	}
}

func readInput(input string) ([]int, []SourceMap) {
	lines := strings.Split(input, "\n")
	i := 0
	var seeds []int
	for _, seed := range strings.Split(strings.TrimPrefix(lines[i], "seeds: "), " ") {
		n, _ := strconv.Atoi(seed)
		seeds = append(seeds, n)
	}
	i += 2

	var sourceMaps []SourceMap
	for i < len(lines) {
		var line string
		line = strings.TrimSpace(lines[i])
		i++
		header := strings.TrimSuffix(line, " map:")
		parts := strings.Split(header, "-")
		sourceType := parts[0]
		destType := parts[2]

		var ranges []RangeMap
		line = strings.TrimSpace(lines[i])
		i++
		for i < len(lines) && len(line) > 0 {
			parts = strings.Split(line, " ")
			destStart, _ := strconv.Atoi(parts[0])
			sourceStart, _ := strconv.Atoi(parts[1])
			size, _ := strconv.Atoi(parts[2])
			ranges = append(ranges, RangeMap{sourceStart, destStart, size})

			line = strings.TrimSpace(lines[i])
			i++
		}

		sourceMaps = append(sourceMaps, SourceMap{sourceType, destType, ranges})
	}

	return seeds, sourceMaps
}

func dump(seeds []int, sourceMaps []SourceMap) {
	fmt.Println(seeds)
	for _, s := range sourceMaps {
		fmt.Println(s.String())
	}
}

func part1(input string) int {
	seeds, sourceMaps := readInput(input)
	for _, sourceMap := range sourceMaps {
		for i := 0; i < len(seeds); i++ {
			seeds[i] = sourceMap.Transform(seeds[i])
		}
	}

	res := seeds[0]
	for i := 1; i < len(seeds); i++ {
		if seeds[i] < res {
			res = seeds[i]
		}
	}

	return res
}

func genSeeds(seedRanges []int, seedChan chan int) {
	done := make(chan bool)
	for i := 0; i < len(seedRanges); i += 2 {
		start, size := seedRanges[i], seedRanges[i+1]
		go func(start, size int) {
			fmt.Printf("Generating Range: start=%d, size=%d\n", start, size)
			for seed := start; seed < start+size; seed++ {
				seedChan <- seed
			}
			fmt.Printf("Finished Range: start=%d, size=%d\n", start, size)
			done <- true
		}(start, size)
	}

	n := 0
	for n < len(seedRanges)/2 {
		<-done
		n++
	}

	close(done)
	close(seedChan)
}

func transformSeeds(sourceMaps []SourceMap, seedChan, outputChan chan int) {
	for seed, ok := <-seedChan; ok; seed, ok = <-seedChan {
		go func(seed int) {
			s := seed
			// fmt.Printf("Transforming Seed: %d\n", s)
			for _, sm := range sourceMaps {
				s = sm.Transform(s)
			}
			outputChan <- s
		}(seed)
	}
	time.Sleep(time.Second)
	close(outputChan)
}

func aggregateSeeds(outputChan, resultChan chan int) {
	res := <-outputChan
	for seed, ok := <-outputChan; ok; seed, ok = <-outputChan {
		if seed < res {
			res = seed
		}
	}
	resultChan <- res
	close(resultChan)
}

// Succeeds, but takes on the order of 15 minutes+ to run
func part2_concurrent(input string) int {
	seedRanges, sourceMaps := readInput(input)

	seedChan := make(chan int)
	go genSeeds(seedRanges, seedChan)

	outputChan := make(chan int)
	go transformSeeds(sourceMaps, seedChan, outputChan)

	resultChan := make(chan int)
	go aggregateSeeds(outputChan, resultChan)

	res := <-resultChan
	return res
}

// func part2(input string) int {
// 	seedRanges, sourceMaps := readInput(input)
// 	var rs []Range
// 	for i := 0; i < len(seedRanges); i += 2 {
// 		rs = append(rs, Range{seedRanges[i], seedRanges[i+1]})
// 	}

// 	res := math.Inf(1)
// 	for _, r := range rs {
// 		var transformedRanges []Range
// 		for _, sourceMap := range sourceMaps {
// 			for _, rm := range sourceMap.ranges {
// 				rrs := rm.SplitRange(r)
// 				for i := 0; i < len(rrs); i++ {
// 					rrs[i] = rm.TransformRange(rrs[i])
// 				}
// 			}
// 		}
// 	}

// 	return res
// }

func main() {
	const DEBUG = true
	filename := "input.txt"
	test_input := `seeds: 79 14 55 13

	seed-to-soil map:
	50 98 2
	52 50 48
	
	soil-to-fertilizer map:
	0 15 37
	37 52 2
	39 0 15
	
	fertilizer-to-water map:
	49 53 8
	0 11 42
	42 0 7
	57 7 4
	
	water-to-light map:
	88 18 7
	18 25 70
	
	light-to-temperature map:
	45 77 23
	81 45 19
	68 64 13
	
	temperature-to-humidity map:
	0 69 1
	1 0 69
	
	humidity-to-location map:
	60 56 37
	56 93 4
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

	fmt.Printf("2023 Day 5, Part 1: %v\n", part1(input))
	fmt.Printf("2023 Day 5, Part 2: %v\n", part2_concurrent(input))
}
