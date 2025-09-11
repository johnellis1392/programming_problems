package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Dir struct {
	name     string
	size     int
	children map[string]Dir
	parent   *Dir
}

func (dir *Dir) getSize() int {
	s := 0
	s += dir.size
	for _, child := range dir.children {
		s += child.getSize()
	}
	return s
}

func (dir *Dir) isLeaf() bool {
	return len(dir.children) == 0
}

func (dir *Dir) calcSizes(max int) int {
	res := 0
	if dir.isLeaf() {
		return 0
	}
	// fmt.Printf("- calc size: '%s'\n", dir.name)
	if dir.getSize() <= max {
		res += dir.getSize()
	}
	for _, child := range dir.children {
		res += child.calcSizes(max)
	}
	return res
}

func (dir *Dir) dumps(indent int) {
	for i := 0; i < indent; i++ {
		fmt.Printf(" ")
	}
	fmt.Printf("- %s (%d)\n", dir.name, dir.getSize())
	for _, child := range dir.children {
		child.dumps(indent + 2)
	}
}

func readInput(input string) Dir {
	lines := strings.Split(input, "\n")
	i := 0
	root := Dir{name: "/", size: 0, parent: nil, children: make(map[string]Dir)}
	cwd := &root
	var line string
outer:
	for i < len(lines) {
		if len(lines[i]) == 0 {
			i++
			continue
		}
		line = strings.TrimSpace(lines[i])
		if line == "$ cd /" {
			cwd = &root
			i++
		} else if line == "$ cd .." {
			cwd = cwd.parent
			i++
		} else if strings.HasPrefix(line, "$ cd") {
			cmd := strings.Split(line, " ")
			dirName := cmd[2]
			if d, ok := cwd.children[dirName]; ok {
				cwd = &d
			} else {
				newDir := Dir{name: dirName, size: 0, parent: cwd, children: make(map[string]Dir)}
				cwd.children[dirName] = newDir
				cwd = &newDir
			}
			i++
		} else if line == "$ ls" {
			i++
			for i < len(lines) && len(lines[i]) != 0 {
				line = strings.TrimSpace(lines[i])
				if strings.HasPrefix(line, "$") {
					continue outer
				}

				parts := strings.Split(line, " ")
				ssize, dirName := parts[0], parts[1]
				size, _ := strconv.Atoi(ssize)

				if d, ok := cwd.children[dirName]; ok {
					d.size = size
				} else {
					newDir := Dir{name: dirName, size: size, parent: cwd, children: make(map[string]Dir)}
					cwd.children[dirName] = newDir
				}

				i++
			}
		}
	}
	return root
}

func part1(input string) int {
	root := readInput(input)
	const MAX = 100000
	res := root.calcSizes(MAX)
	return res
}

func part2(input string) int {
	root := readInput(input)
	const MAX = 70000000
	const MIN = 30000000
	freeSpace := MAX - root.getSize()
	spaceNeeded := MIN - freeSpace
	// fmt.Printf("Space Needed: %d\n", spaceNeeded)
	res := root.getSize()
	var f func(*Dir)
	f = func(d *Dir) {
		if d.isLeaf() {
			return
		}

		s := d.getSize()
		if s >= spaceNeeded && s < res {
			res = s
		}

		for _, child := range d.children {
			f(&child)
		}
	}
	f(&root)
	return res
}

func main() {
	const DEBUG = true
	filename := "input.txt"
	test_input := `$ cd /
	$ ls
	dir a
	14848514 b.txt
	8504156 c.dat
	dir d
	$ cd a
	$ ls
	dir e
	29116 f
	2557 g
	62596 h.lst
	$ cd e
	$ ls
	584 i
	$ cd ..
	$ cd ..
	$ cd d
	$ ls
	4060174 j
	8033020 d.log
	5626152 d.ext
	7214296 k`

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

	fmt.Printf("2022, Day 7, part 1: %v\n", part1(input))
	fmt.Printf("2022, Day 7, part 2: %v\n", part2(input))
}
