package main

import (
	//"errors"
	//"crypto/md5"
	//"encoding/hex"
	"fmt"
	"log"
	"os"
	//"sort"
	//"strconv"
	"strings"
	//	"unicode/utf8"
)

// does not contain the strings ab, cd, pq, or xy,
func containsOutlaw(text string) bool {
	// cheat since only allowed input a-z
	var prev rune = '_'
	for _, ch := range text {
		if prev == 'a' && ch == 'b' {
			return true
		}
		if prev == 'c' && ch == 'd' {
			return true
		}
		if prev == 'p' && ch == 'q' {
			return true
		}
		if prev == 'x' && ch == 'y' {
			return true
		}
		prev = ch
	}
	return false
}

func containsTwiceInARow(text string) bool {
	// cheat since only allowed input a-z
	var prev rune = '_'
	for _, ch := range text {
		if ch == prev {
			return true
		}
		prev = ch
	}
	return false
}

func containsThreeVowels(text string) bool {
	count := 0
	for _, ch := range text {
		if ch == 'a' || ch == 'e' || ch == 'i' || ch == 'o' || ch == 'u' {
			count = count + 1
			if count >= 3 {
				return true
			}
		}
	}
	return false
}

func isNice(text string) bool {
	return containsThreeVowels(text) && containsTwiceInARow(text) && (!containsOutlaw(text))
}

func part1(lines []string) int {
	tot := 0
	for _, line := range lines {
		if line == "" {
		} else {
			if isNice(line) {
				tot = tot + 1
			}
		}
	}
	fmt.Printf("there are %d nice lines\n", tot)
	return tot
}

func main() {

	filePath := "../input.txt"
	content, err := os.ReadFile(filePath)
	if err != nil {
		log.Fatal(err)
	}

	// here we read entire content
	text := string(content)
	lines := strings.Split(text, "\n")

	part1(lines)

}
