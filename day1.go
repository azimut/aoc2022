package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
)

func main() {
	f, err := os.Open("day1.txt")
	if err != nil {
		os.Exit(1)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	elfs := make([][]int, 6)
	elf := make([]int, 0)
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			elfs = append(elfs, elf)
			elf = elf[:0]
			continue
		}
		calories, err := strconv.Atoi(line)
		if err != nil {
			os.Exit(1)
		}
		elf = append(elf, calories)
	}

	sums := make([]int, len(elfs))
	for i := 0; i < len(elfs); i++ {
		sums[i] = sum(elfs[i])
	}
	fmt.Printf("silver:\t%7d\n", silver(sums))
	fmt.Printf("gold:\t%7d\n", gold(sums))

}

func gold(sums []int) int {
	sort.Ints(sums)
	return sum(sums[len(sums)-3:])
}

func silver(sums []int) int {
	return max(sums)
}

func sum(xs []int) (result int) {
	for _, x := range xs {
		result = result + x
	}
	return
}

func max(xs []int) (result int) {
	for _, x := range xs {
		if x > result {
			result = x
		}
	}
	return result
}
