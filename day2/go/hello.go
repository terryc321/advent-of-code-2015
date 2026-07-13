package main

import (
	//	"errors"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"
	//	"unicode/utf8"
)

/*
   input is of the form

   20x3x11
   15x27x5
   6x29x7

*/

func makeCounter() func() int {
	count := 0 // This variable is captured by the closure

	// Return a function that closes over 'count'
	return func() int {
		count++
		return count
	}
}

/*
func wrap(l int, w int, h int) (total int) {
	// we can factor out 2* if we want
	var surface int = 2*l*w + 2*l*h + 2*w*h
	var slack int = min(l*w, min(l*h, w*h))
	total = surface + slack
	// implicit return ? -- still need it ?
	return total
}

*/

// given a list of three numbers from 123x456x789 - compute extra step
func makePart1() (func([]int) int, func() int) {
	total := 0 // This variable is captured by the closure

	// numbers ns given is presorted low to high

	// Return a function that closes over 'total'
	f1 := func(ns []int) int {
		l := ns[0]
		w := ns[1]
		h := ns[2]
		var a int = 2*l*w + 2*l*h + 2*w*h
		var b int = l
		total = total + (a + b)
		return total
	}

	f2 := func() int {
		return total
	}

	return f1, f2
}

/*
func wrap2(n []int) (total int) {

	var a int = n[0] * n[1] * n[2]
	var b int = 2*n[0] + 2*n[1]
	total = a + b
	// implicit return ? -- still need it ?
	return total
}
*/

// given a list of three numbers from 123x456x789 - compute extra step
func makePart2() (func([]int) int, func() int) {
	total := 0 // This variable is captured by the closure

	// numbers ns given is presorted low to high

	// Return a function that closes over 'total'
	f1 := func(ns []int) int {
		l := ns[0]
		w := ns[1]
		h := ns[2]
		var a int = l * w * h
		var b int = 2*l + 2*w
		total = total + (a + b)
		return total
	}
	f2 := func() int {
		return total
	}
	return f1, f2
}

// strconv.ParseInt: num, err := strconv.ParseInt(text, 10, 64)
func split(str string) []int {
	// if i call this i have 3 elements separated by letter x
	parts := strings.Split(str, "x")
	num0, err := strconv.Atoi(parts[0])
	if err != nil {
		fmt.Println("Error converting string to int:", err)
	}
	num1, err := strconv.Atoi(parts[1])
	if err != nil {
		fmt.Println("Error converting string to int:", err)
	}
	num2, err := strconv.Atoi(parts[2])
	if err != nil {
		fmt.Println("Error converting string to int:", err)
	}
	var a int = num0
	var b int = num1
	var c int = num2
	return ([]int{a, b, c})
}

func part(lines []string) {

	// part1 is a function takes 3 numbers as a splice and keeps a running total
	// part1result returns the running total , so we can call part1 from anywhere and
	// not be concerned about losing the running total , we can get that from part1result
	part1, part1result := makePart1()
	part2, part2result := makePart2()

	// iterate over each line
	for _, line := range lines {
		if line == "" {
		} else { // assume contains 123x456x789
			// split by x char
			ns := split(line)
			if len(ns) == 3 {
				sort.Ints(ns)
				// pass to part1
				// pass to part2
				part1(ns)
				part2(ns)
			} else {
				fmt.Printf("error : this line did not have three values\n")
				fmt.Printf("%s ??\nvals=%v\n", line, ns)
			}
		}
	}
	fmt.Printf("part1 : total is %d \n", part1result())
	fmt.Printf("part2 : total is %d \n", part2result())
}

/*
func part2(lines []string) {
	//
	var total int = 0
	// iterate over each line
	for _, line := range lines {
		if line == "" {
		} else { // assume contains 123x456x789
			// split by x char
			ns := split(line)
			// sort them
			sort.Ints(ns)
			total = total + wrap2(ns)
		}
	}
	fmt.Printf("part2 : total is %d \n", total)
}
*/

func main() {
	fmt.Println("Hello, World!")
	filePath := "../input.txt"
	content, err := os.ReadFile(filePath)
	if err != nil {
		log.Fatal(err)
	}

	// here we read entire content
	// fmt.Println("Full file content:")
	text := string(content)
	// fmt.Println(text)
	lines := strings.Split(text, "\n")

	part(lines)

	//part1(lines)
	//part2(lines)

	/*
			for i, line := range lines {
				fmt.Printf("Line %d: %s\n", i, line)
				if i == 1000 {
					fmt.Printf("thousanth Line %d: %s\n", i, line)
					if line == "" {
						fmt.Printf("the thousanth line is indeed empty\n")
					}
				}
			}

		// Get number of runes (characters)
		runes := utf8.RuneCountInString(text)
		fmt.Printf("there are %d runes in the string\n", runes)

		fmt.Printf("min 1 2 3 is %d \n", min(3, min(2, 1)))

		var triple []int = split("14x12x8")
		fmt.Printf("triple = %v", triple)
	*/

	//part1(text)
	//part2(text)
}
