package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	fd, err := os.Open("data/day2.test.txt")
	if err != nil {
		os.Exit(1)
	}
	scanner := bufio.NewScanner(fd)
	for scanner.Scan() {
		line := scanner.Text()
		fmt.Printf("%s\n", line)
	}
}
