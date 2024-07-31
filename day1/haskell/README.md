
* haskell libraries

erm , getting HUnit to work seems like solving a cryptographic problem

```bash
cabal init

answer questions as best see fit
edit generated cabal file add HUnit to depends section

setup Addition module as an executable not a library

executable Addition
   ...
   ...
    build-depends:    base ^>=4.17.2.1 ,
                      HUnit ,
                      QuickCheck 

cabal install HUnit ...... works ok
cabal install QuickCheck  fails because QuickCheck is just a dependency ?



ghci
> :set -package HUnit
> :set -package QuickCheck

> import Test.HUnit
> import Test.QuickCheck

example .....

terry@debian:~/code/advent-of-code/advent-of-code-2015/day1/haskell$ ghci 
Loaded package environment from /home/terry/.ghc/x86_64-linux-9.4.8/environments/default
GHCi, version 9.4.8: https://www.haskell.org/ghc/  :? for help
ghci> :set -package QuickCheck
package flags have changed, resetting and loading new packages...
ghci> :set -package HUnit
package flags have changed, resetting and loading new packages...
ghci> :show packages
active package flags:
  -package HUnit
  -package QuickCheck
  -package-id ieee754-0.8.0-fa4c7cab8e7cb0c7cec008a97e85c7d3e29cad0831ea11be6615c9f7b95e6747
  -package-id Agda-2.6.4.1-b0b4b0897cfb6659b3705f9d0c6581808bb6ba7902fe0e2d889c1da9bc312b40
  -package-id base-4.17.2.1
ghci> 

code editing is in app directory




```

