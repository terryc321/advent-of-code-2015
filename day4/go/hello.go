package main

import (
	//	"errors"
	"crypto/md5"
	"encoding/hex"
	"fmt"
	//"log"
	//"os"
	//"sort"
	//"strconv"
	//"strings"
	//	"unicode/utf8"
)

func md5sum(text string) string {
	hash := md5.New()
	hash.Write([]byte(text))
	sum := hash.Sum(nil)
	return hex.EncodeToString(sum)
}

func part1(text string) int {
	var i int = 0
	var out string
	var test string

	for true {
		// test string
		test = fmt.Sprintf("%s%d", text, i)
		out = md5sum(test)
		// ignore unicode []runes(out)
		if out[0:5] == "00000" {
			fmt.Printf("part1 has solution of %d to give test %s and md5 %s \n", i, test, out)
			return i
		}
		i = i + 1
	}
	return i
}

func part2(text string) int {
	var i int = 0
	var out string
	var test string

	for true {
		// test string
		test = fmt.Sprintf("%s%d", text, i)
		out = md5sum(test)
		// ignore unicode []runes(out)
		if out[0:6] == "000000" {
			fmt.Printf("part2 has solution of %d to give test %s and md5 %s \n", i, test, out)
			return i
		}
		i = i + 1
	}
	return i
}

func main() {
	//fmt.Println("Hello, World!")

	/*
			filePath := "../input.txt"
			content, err := os.ReadFile(filePath)
			if err != nil {
				log.Fatal(err)
			}


		// compute md5 of "abcdef609043"
		var text string = "abcdef609043"
		// hash := md5.New()
		// hash.Write([]byte(text))
		// sum := hash.Sum(nil)
		// md5String := hex.EncodeToString(sum)
		fmt.Printf("MD5 of '%s': %s\n", text, md5sum(text))

		text = "pqrstuv1048970"
		fmt.Printf("MD5 of '%s': %s\n", text, md5sum(text))

		// try
		//part1("abcdef")
		//part1("pqrstuv")
	*/

	part1("bgvyzdsv")
	part2("bgvyzdsv")

	// here we read entire content
	//fmt.Println("Full file content:")
	//text := string(content)
	//fmt.Println(text)
	//fmt.Printf("there are %d characters\n", len(text))
	//lines := strings.Split(text, "\n")

	//part1(text)
	//part2(text)

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
