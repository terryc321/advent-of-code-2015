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

func wrap(l int, w int, h int) (total int) {
	// we can factor out 2* if we want
	var surface int = 2*l*w + 2*l*h + 2*w*h
	var slack int = min(l*w, min(l*h, w*h))
	total = surface + slack
	// implicit return ? -- still need it ?
	return total
}

func part1(lines []string) {
	//
	var total int = 0
	// iterate over each line
	for _, line := range lines {
		if line == "" {
		} else { // assume contains 123x456x789
			// split by x char
			ns := split(line)
			total = total + wrap(ns[0], ns[1], ns[2])
		}
	}
	fmt.Printf("part1 : total is %d \n", total)
	//fmt.Println(level)
}

func wrap2(n []int) (total int) {

	var a int = n[0] * n[1] * n[2]
	var b int = 2*n[0] + 2*n[1]
	total = a + b
	// implicit return ? -- still need it ?
	return total
}

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

func main() {
	fmt.Println("Hello, World!")
	filePath := "../input.txt"
	content, err := os.ReadFile(filePath)
	if err != nil {
		log.Fatal(err)
	}

	// fmt.Println("Full file content:")
	text := string(content)
	// fmt.Println(text)
	lines := strings.Split(text, "\n")

	part1(lines)
	part2(lines)

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
