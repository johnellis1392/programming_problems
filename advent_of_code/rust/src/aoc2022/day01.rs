use std::fs::{File};
use std::io::{self, BufRead, BufReader};

fn day01_part1(filename: &str) -> i32 {
    // let file = fs::read_to_string(filename)
    //     .expect("An error occurred while reading file");
    // println!("File Contents: {}", file);
    let file: File = File::open(filename).expect("Failed to open file");
    let lines: io::Lines<BufReader<File>> = io::BufReader::new(file).lines();
    let mut results: Vec<i32> = Vec::new();
    let mut values: Vec<i32> = Vec::new();

    for line in lines {
        if let Some(r) = line.ok().and_then(|v| { v.parse::<i32>().ok() }) {
            values.push(r);
        } else {
            results.push(values.iter().sum());
            values.clear();
        }
    }

    results.push(values.iter().sum());
    results.sort();
    results.pop().expect("An error occurred while popping result list")
}

fn day01_part2(filename: &str) -> i32 {
    let file: File = File::open(filename).expect("Failed to open file");
    let lines = io::BufReader::new(file).lines();
    let mut results: Vec<i32> = Vec::new();
    let mut values: Vec<i32> = Vec::new();

    for line in lines {
        if let Some(r) = line.ok().and_then(|v| { v.parse::<i32>().ok() }) {
            values.push(r);
        } else {
            results.push(values.iter().sum());
            values.clear();
        }
    }

    results.push(values.iter().sum());
    results.sort();
    // results.iter().take(3).sum()
    results.iter().skip(results.len() - 3).sum()
}

fn main() {
    // let input_filename: &str = "input.test.txt";
    let input_filename: &str = "input.txt";
    let result1: i32 = day01_part1(input_filename);
    println!("Result1: {}", result1);
    let result2: i32 = day01_part2(input_filename);
    println!("Result2: {}", result2);
}
