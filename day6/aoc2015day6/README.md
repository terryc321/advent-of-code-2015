# aoc2015day6

using the ST monad we create a mutable pocket universe do some stuff then return an immutable
result , having speed and efficiency combined with type system ensures it cannot escape and 
pollute anything else

idea is create a big 1000 x 1000 grid , do our mutation to our grid like ordinary common lisp ,
then when finished return the end result all cleaned up

```
stack repl

> let grid = createGrid
> print $ grid ! (0, 0)      -- False
> print $ grid ! (999, 999)  -- False 
```

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

