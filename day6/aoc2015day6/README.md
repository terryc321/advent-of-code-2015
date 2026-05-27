# aoc2015day6

Parsec looks nice , spaces consumes newlines apparently , worked a while getting test fixtures to fail and fail loudly

```
stack test
```


```
the grammar

program     := line*
line        := command space range
command     := "turn on" | "turn off" | "toggle"
range       := start through end
start       := coord
end         := coord
through     := " through "
coord       := number "," number
number      := digit+
```

Haskell parsec after several iterations and ironing out bugs from leo ai slop


```
package.yaml changes when running Stack for parsec 

  - parsec
  - text
  - containers
```

```
megaparsec is more modern version of parsec - advised i use this instead 
```

