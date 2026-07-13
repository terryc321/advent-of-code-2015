package main

import "fmt"

import (
	"log"
	"os"
)

func part1(text string) {
 	// level := 0 
	var level int = 0 
	for _, runeValue := range text {
		if runeValue == '(' {
		     level = level + 1  
		  } else if runeValue == ')' {
		     level = level - 1  
		  } else {
		  // no idea 
		   }
	}
	fmt.Print("part1 : level is ")
	fmt.Println(level)
}

func part2(text string){

	// part 2 
	var idx int = 0
	var level int = 0 
	for _, runeValue := range text {
		if runeValue == '(' {
		     level = level + 1
		     idx = idx + 1
		  } else if runeValue == ')' {
		     level = level - 1
		     idx = idx + 1 
		  } else {
		  // no idea 
		   }
		   if level < 0 {
		    break
		   }
	}
	fmt.Print("part2 : level is ")
	fmt.Println(idx)

}

func main() {
	fmt.Println("Hello, World!")
	filePath := "../input"
	content, err := os.ReadFile(filePath)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println("Full file content:")
	text := string(content)
	fmt.Println(text)

	part1(text)
	part2(text)
}

