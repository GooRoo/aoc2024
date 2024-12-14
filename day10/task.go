package main

import (
	"fmt"
	"os"
	"strings"
)

const TRAILHEAD = 0
const TRAILEND = 9

func main() {
	solveFirst()
	solveSecond()
}

func solveFirst() {
	matrix := readData("data/task.data")

	score := 0
	for y, row := range matrix.data {
		for x, cell := range row {
			if cell == TRAILHEAD {
				ends := findPath(matrix, point{y, x})
				fmt.Println("Starting at", point{y, x}, "- found", len(ends), "ends")
				score += len(ends)
			}
		}
	}

	fmt.Println("Score", score)
}

func solveSecond() {
	matrix := readData("data/task.data")

	rating := 0
	for y, row := range matrix.data {
		for x, cell := range row {
			if cell == TRAILHEAD {
				ends := findPath2(matrix, point{y, x})
				fmt.Println("Starting at", point{y, x}, "- found", len(ends), "ends")
				rating += len(ends)
			}
		}
	}

	fmt.Println("Rating", rating)
}

func findPath(matrix theMap, start point) (ends map[point]struct{}) {
	ends = make(map[point]struct{})

	if matrix.data[start.row][start.col] == TRAILEND {
		ends[start] = struct{}{}
		return
	}

	for _, next := range getNeighbors(matrix, start) {
		c := matrix.data[start.row][start.col]
		n := matrix.data[next.row][next.col]
		if n == c + 1 {
			ends = mergeSets(ends, findPath(matrix, next))
		}
	}

	return
}

func findPath2(matrix theMap, start point) (ends []point) {
	ends = make([]point, 0)

	if matrix.data[start.row][start.col] == TRAILEND {
		ends = append(ends, start)
		return
	}

	for _, next := range getNeighbors(matrix, start) {
		c := matrix.data[start.row][start.col]
		n := matrix.data[next.row][next.col]
		if n == c + 1 {
			ends = append(ends, findPath2(matrix, next)...)
		}
	}

	return
}

func getNeighbors(matrix theMap, p point) []point {
	maybeNeighbors := []point{
		{p.row - 1, p.col},
		{p.row + 1, p.col},
		{p.row, p.col - 1},
		{p.row, p.col + 1},
	}

	return filter(maybeNeighbors, func(p point) bool {
		return p.row >= 0 && p.row < matrix.height && p.col >= 0 && p.col < matrix.width
	})
}

func filter[T any](input []T, predicate func(T) bool) []T {
    result := []T{}
    for _, item := range input {
        if predicate(item) {
            result = append(result, item)
        }
    }
    return result
}

func mergeSets(set1, set2 map[point]struct{}) map[point]struct{} {
    result := make(map[point]struct{})

    for p := range set1 {
        result[p] = struct{}{}
    }

    for p := range set2 {
        result[p] = struct{}{}
    }

    return result
}

func readData(filename string) theMap {
	contents, err := os.ReadFile(filename)
	if err != nil {
		fmt.Println("Error reading file", err)
		os.Exit(1)
	}

	lines := strings.Split(strings.TrimSpace(string(contents)), "\n")
	width, height := len(lines[0]), len(lines)
	data := make([][]int, height)
	for y, line := range lines {
		if line == "" { break }
		data[y] = make([]int, width)
		for x, char := range line {
			data[y][x] = int(char - '0')
		}
	}
	return theMap{data, width, height}
}

type theMap struct {
	data [][]int
	width, height int
}

type point struct {
	row, col int
}
