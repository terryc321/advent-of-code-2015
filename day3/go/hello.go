package main

import (
	//	"errors"
	"fmt"
	"log"
	"os"
	//"sort"
	//"strconv"
	//"strings"
	//	"unicode/utf8"
)

// define coordinate
type Coord struct {
	X int
	Y int
}

type CoordSet map[Coord]struct{}

func (ps CoordSet) Has(p Coord) bool {
	_, ok := ps[p]
	return ok
}

func (ps CoordSet) Add(p Coord) {
	ps[p] = struct{}{}
}

func deliver(p Coord) {
	fmt.Printf("global delivery to coordinate (%d , %d)\n", p.X, p.Y)
}

// // Usage
// points := make(CoordSet)
// points.Add(Coord{50, 20})

//	if points.Has(Coord{50, 20}) {
//	    fmt.Println("Found it!")
//	}
func makeSanta() func(rune) CoordSet {
	var x int = 0
	var y int = 0
	points := make(CoordSet)
	deliver := func(p Coord) { points.Add(p) }
	deliver(Coord{x, y})
	santa := func(ch rune) CoordSet {

		if ch == '^' {
			y = y - 1
			deliver(Coord{x, y})
		} else if ch == 'v' {
			y = y + 1
			deliver(Coord{x, y})
		} else if ch == '<' {
			x = x - 1
			deliver(Coord{x, y})
		} else if ch == '>' {
			x = x + 1
			deliver(Coord{x, y})
		}

		return points
	}
	return santa
}

/*

	// how many houses get atleast one present?
	// show me
	// is (41,86) in the delivery?
	if points.Has(Coord{41, 86}) {
		fmt.Println("has 41 86")
	}

	count := len(points)
	fmt.Printf("there are %d deliveries \n", count)

	return count

*/

func part2(text string) int {
	//var odds , evens []int
	var santa = makeSanta()
	var robo = makeSanta()
	s := santa(' ')
	r := robo(' ')

	for i, ch := range text {
		if i%2 == 0 {
			//evens = append(evens, ch)
			s = santa(ch)
		} else {
			//odds = append(odds, ch)
			r = robo(ch)
		}
	}
	// Merge map2 into map1
	for k, v := range r {
		s[k] = v
	}

	count := len(s)
	fmt.Printf("there are %d deliveries \n", count)
	return count
}

func part1(text string) int {
	santa := makeSanta()
	// give santa a dummy command he will ignore and get the points out
	points := santa(' ')
	for _, ch := range text {
		points = santa(ch)
	}

	// how many houses get atleast one present?
	// show me
	// is (41,86) in the delivery?
	if points.Has(Coord{41, 86}) {
		fmt.Println("has 41 86")
	}

	count := len(points)
	fmt.Printf("there are %d deliveries \n", count)
	return count
}

func main() {
	fmt.Println("Hello, World!")
	filePath := "../input.txt"
	content, err := os.ReadFile(filePath)
	if err != nil {
		log.Fatal(err)
	}

	// here we read entire content
	//fmt.Println("Full file content:")
	text := string(content)
	//fmt.Println(text)
	//fmt.Printf("there are %d characters\n", len(text))
	//lines := strings.Split(text, "\n")

	part1(text)
	part2(text)

	//part(lines)

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
