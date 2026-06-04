# README.md

## haskell

```
cd aoc2015day6
stack build
time stack run

Parsed 300 instructions.
First 5:
Instruction {cmd = Toggle, start = Coord {x = 461, y = 550}, end = Coord {x = 564, y = 900}}
Instruction {cmd = TurnOff, start = Coord {x = 370, y = 39}, end = Coord {x = 425, y = 839}}
Instruction {cmd = TurnOff, start = Coord {x = 464, y = 858}, end = Coord {x = 833, y = 915}}
Instruction {cmd = TurnOff, start = Coord {x = 812, y = 389}, end = Coord {x = 865, y = 874}}
Instruction {cmd = TurnOn, start = Coord {x = 599, y = 989}, end = Coord {x = 806, y = 993}}
Light at (0,0): False
Light at (500,500): True
Lights that are on: 543903
Total brightness : 14687245

real	0m0.401s
user	0m0.342s
sys	0m0.068s
```

## ocaml 

```
cd day6/ocaml/day6
dune build
time dune exec day6


part1 solution 543903             
part2 solution 14687245

real    0m0.153s
user    0m0.097s
sys     0m0.043s
```