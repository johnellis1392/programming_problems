package main

import (
  "bufio"
  "fmt"
  "log"
  "os"
  "sort"
  "strconv"
)

func day01_part1(filename string) int {
  var result int = 0
  file, err := os.Open(filename)
  if err != nil {
    log.Fatal(err)
  }
  defer file.Close()

  scanner := bufio.NewScanner(file)
  sum := 0
  for scanner.Scan() {
    s := scanner.Text()
    if s == "" {
      sum = 0
      continue
    }

    i, err := strconv.Atoi(s)
    if err != nil {
      log.Fatal(err)
    }

    sum += i
    if sum > result {
      result = sum
    }
  }

  if err := scanner.Err(); err != nil {
    log.Fatal(err)
  }

  return result
}

func day01_part2(filename string) int {
  var result int = 0
  file, err := os.Open(filename)
  if err != nil {
    log.Fatal(err)
  }
  defer file.Close()

  var sum int = 0
  var results []int = nil
  var values []int = nil

  scanner := bufio.NewScanner(file)
  for scanner.Scan() {
    s := scanner.Text()
    if s == "" {
      sum = 0
      for _, v := range values {
        sum += v
      }
      results = append(results, sum)
      values = nil
      continue
    }

    i, err := strconv.Atoi(s)
    if err != nil {
      log.Fatal(err)
    }

    values = append(values, i)
  }

  sum = 0
  for _, v := range values {
    sum += v
  }
  results = append(results, sum)
  values = nil

  sort.Slice(results, func(i, j int) bool { return results[j] < results[i] })

  result = 0
  for _, v := range results[:3] {
    result += v
  }

  return result
}

func main() {
  // input_filename := "input.test.txt"
  input_filename := "input.txt"
  result1 := day01_part1(input_filename)
  fmt.Printf("Result1: %d\n", result1)
  result2 := day01_part2(input_filename)
  fmt.Printf("Result2: %d\n", result2)
}
